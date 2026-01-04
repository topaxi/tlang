#![feature(if_let_guard)]
#![feature(box_patterns)]
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use log::debug;
use smallvec::SmallVec;
use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token;
use tlang_hir::hir::{self, BindingKind};
use tlang_memory::shape::{ShapeKey, Shaped, TlangEnumVariant, TlangShape};
use tlang_memory::state::TailCall;
use tlang_memory::value::TlangArithmetic;
use tlang_memory::value::object::TlangEnum;
use tlang_memory::{InterpreterState, NativeFnReturn, Resolver, scope};
use tlang_memory::{prelude::*, state};
use tlang_span::HirId;
use tlang_symbols::SymbolType;

pub use tlang_memory::NativeFnDef;

mod macros;

#[derive(Debug, Clone, Copy)]
pub enum EvalResult {
    Void,
    Value(TlangValue),
    Return(TlangValue),
    TailCall,
    Continue,
    Break(TlangValue),
}

impl EvalResult {
    fn unwrap_value(self) -> TlangValue {
        match self {
            EvalResult::Value(value) => value,
            EvalResult::Return(value) => value,
            EvalResult::Break(value) => value,
            EvalResult::Void => TlangValue::Nil,
            EvalResult::TailCall => panic!("Tried to unwrap a TailCall"),
            EvalResult::Continue => panic!("Tried to unwrap Continue"),
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
}

impl Resolver for Interpreter {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue> {
        self.state.resolve_value(path)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    fn builtin_module_symbols() -> Vec<(&'static str, SymbolType)> {
        let mut module_names = HashSet::new();

        inventory::iter::<NativeFnDef>
            .into_iter()
            .map(|def| def.module())
            .filter(|module_name| module_names.insert(module_name.to_string()))
            .map(|module_name| (module_name, SymbolType::Module))
            .collect()
    }

    fn builtin_fn_symbols() -> Vec<(String, SymbolType)> {
        inventory::iter::<NativeFnDef>
            .into_iter()
            .map(|def| (def.name(), SymbolType::Function(def.arity() as u16)))
            .collect::<Vec<_>>()
    }

    const fn builtin_const_symbols() -> &'static [(&'static str, SymbolType)] {
        &[
            ("Option", SymbolType::Enum),
            ("Option::None", SymbolType::EnumVariant(0)),
            ("Result", SymbolType::Enum),
            ("math::pi", SymbolType::Variable),
        ]
    }

    pub fn builtin_symbols() -> Vec<(String, SymbolType)> {
        let mut symbols: Vec<(String, SymbolType)> = Self::builtin_module_symbols()
            .iter()
            .map(|(name, ty)| (name.to_string(), *ty))
            .collect();
        symbols.extend(Self::builtin_fn_symbols());
        symbols.extend(
            Self::builtin_const_symbols()
                .iter()
                .map(|(name, ty)| (name.to_string(), *ty)),
        );
        symbols
    }

    /// # Panics
    pub fn new() -> Self {
        let mut interpreter = Self {
            state: InterpreterState::default(),
        };

        interpreter.init_stdlib();

        for native_fn_def in inventory::iter::<NativeFnDef> {
            interpreter.define_native_fn(&native_fn_def.name(), native_fn_def.fn_ptr());
        }

        interpreter.state.set_global(
            "math::pi".to_string(),
            TlangValue::F64(std::f64::consts::PI),
        );

        interpreter
    }

    #[cfg(feature = "stdlib")]
    pub fn init_stdlib(&mut self) {
        tlang_stdlib::option::define_option_shape(&mut self.state);
        tlang_stdlib::result::define_result_shape(&mut self.state);
        tlang_stdlib::collections::define_list_shape(&mut self.state);
    }

    #[cfg(not(feature = "stdlib"))]
    pub fn init_stdlib(&mut self) {}

    pub fn state(&self) -> &InterpreterState {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut InterpreterState {
        &mut self.state
    }

    fn panic(&self, message: String) -> ! {
        self.state.panic(message)
    }

    pub fn create_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        self.state.new_native_fn(name, f)
    }

    pub fn define_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.create_native_fn(name, f);

        debug!("Defining global native function: {name}");

        self.state.set_global(name.to_string(), fn_object);

        fn_object
    }

    fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.state.get_closure_decl(id)
    }

    fn get_object_by_id(&self, id: TlangObjectId) -> &TlangObjectKind {
        self.state.get_object_by_id(id).unwrap()
    }

    fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        self.state.get_object(value)
    }

    fn get_struct(&self, value: TlangValue) -> Option<&TlangStruct> {
        self.state.get_struct(value)
    }

    fn get_struct_mut(&mut self, value: TlangValue) -> Option<&mut TlangStruct> {
        self.state.get_struct_mut(value)
    }

    fn get_enum(&self, value: TlangValue) -> Option<&TlangEnum> {
        self.state.get_enum(value)
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

    fn get_shape_of(&self, value: TlangValue) -> Option<&TlangShape> {
        match self.get_object(value)?.shape() {
            Some(s) => self.state.get_shape_by_key(s),
            _ => self.panic(format!("Cannot get shape of non-struct object: {value:?}")),
        }
    }

    fn get_str(&self, value: TlangValue) -> Option<&str> {
        self.get_object(value).and_then(|obj| obj.get_str())
    }

    #[inline(always)]
    fn push_value(&mut self, value: TlangValue) {
        self.state.scope_stack.push_value(value);
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
    fn with_new_fn_scope<F, R>(&mut self, fn_decl: &Rc<hir::FunctionDeclaration>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.state
            .push_call_stack(state::CallStackEntry::new_call(fn_decl));
        let result = self.with_new_scope(fn_decl, f);
        self.state.pop_call_stack();
        result
    }

    #[inline(always)]
    fn with_root_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let root_scope = vec![*self.state.scope_stack.root_scope()];
        self.with_scope(root_scope, f)
    }

    #[inline(always)]
    fn with_scope<F, R>(&mut self, scopes: Vec<scope::Scope>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_scopes = std::mem::replace(&mut self.state.scope_stack.scopes, scopes);
        let result = f(self);
        self.state.scope_stack.scopes = old_scopes;
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

    fn eval_loop(&mut self, block: &hir::Block) -> EvalResult {
        loop {
            match self.eval_block(block) {
                EvalResult::Break(value) => return EvalResult::Value(value),
                EvalResult::Return(value) => return EvalResult::Return(value),
                EvalResult::TailCall => return EvalResult::TailCall,
                EvalResult::Continue | EvalResult::Value(_) | EvalResult::Void => {}
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &hir::Stmt) -> EvalResult {
        self.state.set_current_span(stmt.span);

        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.eval_expr(expr),
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.eval_fn_decl(decl);
                EvalResult::Void
            }
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.eval_dyn_fn_decl(decl);
                EvalResult::Void
            }
            hir::StmtKind::StructDeclaration(decl) => {
                self.eval_struct_decl(decl);
                EvalResult::Void
            }
            hir::StmtKind::EnumDeclaration(decl) => {
                self.eval_enum_decl(decl);
                EvalResult::Void
            }
            hir::StmtKind::Let(pat, expr, ty) => self.eval_let_stmt(pat, expr, ty),
            hir::StmtKind::Return(Some(expr)) => {
                EvalResult::Return(eval_value!(self.eval_expr(expr)))
            }

            hir::StmtKind::Return(_) => EvalResult::Return(TlangValue::Nil),
        }
    }

    fn eval_stmts(&mut self, stmts: &[hir::Stmt]) -> EvalResult {
        for stmt in stmts {
            propagate!(self.eval_stmt(stmt));
        }

        EvalResult::Void
    }

    /// Evaluate an expression and return the result.
    /// This is useful for benchmarking and testing individual expressions.
    pub fn eval_expr(&mut self, expr: &hir::Expr) -> EvalResult {
        self.state.set_current_span(expr.span);

        match &expr.kind {
            hir::ExprKind::Path(path) => {
                EvalResult::Value(self.resolve_value(path).unwrap_or_else(|| {
                    self.panic(format!(
                        "Could not resolve path \"{}\" ({:?})\nCurrent scope: {}",
                        path,
                        path.res,
                        self.state.debug_stringify_scope_stack()
                    ))
                }))
            }
            hir::ExprKind::Literal(value) => EvalResult::Value(self.eval_literal(value)),
            hir::ExprKind::List(values) => self.eval_list_expr(values),
            hir::ExprKind::Dict(entries) => self.eval_dict_expr(entries),
            hir::ExprKind::IndexAccess(lhs, rhs) => self.eval_index_access(lhs, rhs),
            hir::ExprKind::FieldAccess(lhs, rhs) => self.eval_field_access(lhs, rhs),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Loop(block) => self.eval_loop(block),
            hir::ExprKind::Break(Some(expr)) => {
                EvalResult::Break(eval_value!(self.eval_expr(expr)))
            }
            hir::ExprKind::Break(_) => EvalResult::Break(TlangValue::Nil),
            hir::ExprKind::Continue => EvalResult::Continue,
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
            Some(TlangObjectKind::Struct(obj)) => EvalResult::Value(obj[rhs_value.as_usize()]),
            Some(TlangObjectKind::Slice(slice)) => {
                EvalResult::Value(self.get_slice_value(*slice, rhs_value.as_usize()))
            }
            _ => todo!("eval_index_access: {:?}[{:?}]", lhs, rhs_value),
        }
    }

    fn eval_field_access(&mut self, lhs: &hir::Expr, ident: &Ident) -> EvalResult {
        let value = eval_value!(self.eval_expr(lhs));

        if let Some(TlangObjectKind::Struct(obj)) = self.get_object(value) {
            if let Some(index) = self
                .state
                .get_struct_field_index(obj.shape(), ident.as_str())
            {
                return EvalResult::Value(obj[index]);
            }

            let shape = self
                .state
                .get_shape(obj)
                .and_then(|shape| shape.get_struct_shape())
                .unwrap();

            self.panic(format!(
                "Could not find field `{}` on {}",
                ident, shape.name
            ));
        }

        if value.is_nil() {
            self.panic(format!("Cannot access field `{ident}` on nil"));
        }

        if !value.is_object() {
            self.panic(format!(
                "Cannot access field `{ident}` on non-object: {}",
                self.state.stringify(value)
            ));
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

                    debug!(
                        "eval_binary: {:?} && {:?}",
                        self.state.stringify(lhs),
                        self.state.stringify(rhs)
                    );

                    if rhs.is_truthy(&self.state) {
                        return EvalResult::Value(TlangValue::Bool(true));
                    }
                }

                debug!("eval_binary: {:?} && ...", self.state.stringify(lhs));

                return EvalResult::Value(TlangValue::Bool(false));
            }

            hir::BinaryOpKind::Or => {
                let lhs = eval_value!(self.eval_expr(lhs));

                if lhs.is_truthy(&self.state) {
                    debug!("eval_binary: {:?} || ...", self.state.stringify(lhs));

                    return EvalResult::Value(TlangValue::Bool(true));
                }

                return self.eval_expr(rhs);
            }

            hir::BinaryOpKind::Assign if let hir::ExprKind::Path(path) = &lhs.kind => {
                let value = eval_value!(self.eval_expr(rhs));

                debug!("eval_binary: {} = {}", path, self.state.stringify(value));

                self.state.scope_stack.update_value(&path.res, value);

                return EvalResult::Value(value);
            }

            hir::BinaryOpKind::Assign
                if let hir::ExprKind::FieldAccess(base, ident) = &lhs.kind =>
            {
                let struct_value = eval_value!(self.eval_expr(base));
                let struct_shape = self
                    .get_object(struct_value)
                    .and_then(|o| o.shape())
                    .unwrap_or_else(|| {
                        self.panic(format!("Cannot assign to field `{ident}` on non-object"))
                    });
                let index = self
                    .state
                    .get_struct_field_index(struct_shape, ident.as_str())
                    .unwrap_or_else(|| {
                        self.panic(format!(
                            "Cannot assign to field `{ident}` on struct `{struct_shape:?}`"
                        ))
                    });

                let value = eval_value!(self.eval_expr(rhs));

                let struct_obj = self.get_struct_mut(struct_value).unwrap();

                struct_obj[index] = value;

                return EvalResult::Value(value);
            }

            hir::BinaryOpKind::Assign => {
                todo!("eval_binary: Assign not implemented for {:?}", lhs);
            }

            _ => {}
        }

        let lhs = eval_value!(self.eval_expr(lhs));
        let rhs = eval_value!(self.eval_expr(rhs));

        debug!(
            "eval_binary: {:?} {:?} {:?}",
            self.state.stringify(lhs),
            op,
            self.state.stringify(rhs)
        );

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

            hir::BinaryOpKind::Assign | hir::BinaryOpKind::And | hir::BinaryOpKind::Or => {
                unreachable!("{:?} should be handled before", op)
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

                        let obj_lhs = self.get_object_by_id(lhs_id);
                        let obj_rhs = self.get_object_by_id(rhs_id);

                        match (obj_lhs, obj_rhs) {
                            (TlangObjectKind::Struct(lhs), TlangObjectKind::Struct(rhs)) => {
                                if lhs.shape() != rhs.shape() {
                                    return EvalResult::Value(TlangValue::Bool(false ^ is_not));
                                }

                                // TODO: Implement comparisons between structs of the same shape.
                                EvalResult::Value(TlangValue::Bool(false ^ is_not))
                            }
                            (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                                EvalResult::Value(TlangValue::Bool((lhs == rhs) ^ is_not))
                            }
                            _ => todo!(
                                "eval_object_binary_op: {:?}, {:?}, {:?}",
                                op,
                                self.state.stringify(lhs),
                                self.state.stringify(rhs)
                            ),
                        }
                    }
                    _ => todo!(
                        "eval_object_binary_op: {:?}, {:?}, {:?}",
                        op,
                        self.state.stringify(lhs),
                        self.state.stringify(rhs)
                    ),
                }
            }
            hir::BinaryOpKind::Add => match (lhs, rhs) {
                (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                    let obj_lhs = self.get_object_by_id(lhs_id);
                    let obj_rhs = self.get_object_by_id(rhs_id);

                    match (obj_lhs, obj_rhs) {
                        (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                            EvalResult::Value(self.state.new_string(lhs.clone() + rhs))
                        }
                        _ => todo!(
                            "eval_object_binary_op: {:?}, {:?}, {:?}",
                            op,
                            self.state.stringify(lhs),
                            self.state.stringify(rhs)
                        ),
                    }
                }
                _ => todo!(
                    "eval_object_binary_op: {:?}, {:?}, {:?}",
                    op,
                    self.state.stringify(lhs),
                    self.state.stringify(rhs)
                ),
            },
            _ => todo!(
                "eval_object_binary_op: {:?}, {:?}, {:?}",
                op,
                self.state.stringify(lhs),
                self.state.stringify(rhs)
            ),
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
        self.state.set_fn_decl(decl.hir_id, Rc::new(decl.clone()));

        let fn_object = self.state.new_object(TlangObjectKind::Fn(decl.hir_id));

        if self.state.is_global_scope() || !self.state.current_scope_has_slots() {
            self.push_value(fn_object);
        } else {
            self.state.set_let_binding(fn_object);
        }

        match &decl.name.kind {
            hir::ExprKind::Path(path) => {
                // Used for static struct method resolution, for now..
                self.state.set_global(path.to_string(), fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_fn_decl: {:?}", expr),
                };

                // Used for static struct method resolution, for now..
                self.state
                    .set_global(path.to_string() + ident.as_str(), fn_object);

                match &path.res.binding_kind() {
                    BindingKind::Struct => {
                        let struct_decl = self.state.get_struct_decl(path).unwrap();

                        self.state.set_struct_method(
                            struct_decl.hir_id.into(),
                            ident.as_str(),
                            TlangStructMethod::HirId(decl.hir_id),
                        );
                    }
                    BindingKind::Enum => {
                        let enum_decl = self.state.get_enum_decl(path).unwrap();

                        self.state.set_enum_method(
                            enum_decl.hir_id.into(),
                            ident.as_str(),
                            TlangStructMethod::HirId(decl.hir_id),
                        );
                    }
                    BindingKind::Unknown => {
                        self.panic(format!(
                            "Could not define method {ident} on unresolved path: {path:?}"
                        ));
                    }
                    _ => todo!("eval_fn_decl: {:?}", path),
                }
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn create_dyn_fn_object(&mut self, decl: &hir::DynFunctionDeclaration) -> TlangValue {
        let name = match &decl.name.kind {
            hir::ExprKind::Path(path) => path.to_string(),
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("create_dyn_fn_object: {:?}", expr),
                };

                path.join_with(ident.as_str())
            }
            _ => todo!("create_dyn_fn_object: {:?}", decl.name),
        };
        let variants = decl.variants.clone();

        self.create_native_fn(&(name.clone() + "/*"), move |state, args| {
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
                state.panic(format!("Function {name} not found"))
            }
        })
    }

    fn eval_dyn_fn_decl(&mut self, decl: &hir::DynFunctionDeclaration) {
        let dyn_fn_object = self.create_dyn_fn_object(decl);

        if self.state.is_global_scope() || !self.state.current_scope_has_slots() {
            self.push_value(dyn_fn_object);
        } else {
            self.state.set_let_binding(dyn_fn_object);
        }

        match &decl.name.kind {
            hir::ExprKind::Path(path) => {
                // Used for static struct method resolution, for now..
                self.state.set_global(path.to_string(), dyn_fn_object);
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
            .set_struct_decl(decl.name.to_string(), Rc::new(decl.clone()));

        let struct_shape = TlangShape::new_struct_shape(
            decl.name.to_string(),
            decl.fields
                .iter()
                .map(|field| field.name.to_string())
                .collect(),
            HashMap::new(),
        );

        self.state.set_shape(decl.hir_id.into(), struct_shape);
    }

    fn eval_enum_decl(&mut self, decl: &hir::EnumDeclaration) {
        self.state
            .set_enum_decl(decl.name.to_string(), Rc::new(decl.clone()));

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
        // TODO: AND has no functions attached to it.

        if is_simple_enum {
            for (value, _variant) in decl.variants.iter().enumerate() {
                self.push_value(TlangValue::from(value));
            }
        } else {
            let variant_shapes = decl
                .variants
                .iter()
                .map(|variant| {
                    let variant_name = variant.name.to_string();
                    TlangEnumVariant::new(
                        variant_name,
                        variant
                            .parameters
                            .iter()
                            .enumerate()
                            .map(|(i, param)| (param.name.to_string(), i))
                            .collect(),
                    )
                })
                .collect::<Vec<_>>();

            let shape_key = decl.hir_id.into();

            for (variant_index, _) in variant_shapes
                .iter()
                .enumerate()
                .filter(|(_, v)| v.field_map.is_empty())
            {
                let enum_value = self.state.new_object(TlangObjectKind::Enum(TlangEnum::new(
                    shape_key,
                    variant_index,
                    vec![],
                )));

                self.push_value(enum_value);
            }

            let enum_shape =
                TlangShape::new_enum_shape(decl.name.to_string(), variant_shapes, HashMap::new());

            self.state.set_shape(shape_key, enum_shape);
        }
    }

    fn eval_call_object(&mut self, callee: TlangValue, args: &[TlangValue]) -> TlangValue {
        let id = callee
            .get_object_id()
            .unwrap_or_else(|| self.panic(format!("`{callee:?}` is not a function")));

        match self.get_object_by_id(id) {
            TlangObjectKind::Closure(closure) => {
                let closure_decl = self.get_closure_decl(closure.id).unwrap();
                // Clone the captured scope stack so we can move it into `with_scope`
                // without borrowing `closure` for the duration of the call. The closure
                // already owns its own independent `scope_stack`; this clone exists
                // solely to satisfy borrowing/ownership requirements, not to preserve
                // the original scope stack.
                let scope_stack = closure.scope_stack.clone();

                self.with_scope(scope_stack, |this| {
                    this.eval_fn_call(&closure_decl, callee, args)
                        .unwrap_value()
                })
            }
            TlangObjectKind::Fn(hir_id) => {
                let fn_decl = self
                    .state
                    .get_fn_decl(*hir_id)
                    .unwrap_or_else(|| self.panic("Function not found".to_string()));

                self.with_root_scope(|this| {
                    this.eval_fn_call(&fn_decl, callee, args).unwrap_value()
                })
            }
            TlangObjectKind::NativeFn => self.exec_native_fn(id, callee, args),
            obj => self.panic(format!("`{obj:?}` is not a function")),
        }
    }

    fn eval_tail_call(&mut self, call_expr: &hir::CallExpression) -> EvalResult {
        if call_expr.has_wildcard() {
            self.panic("Tail call with wildcard arguments not allowed".to_string());
        }

        let callee = eval_value!(self.eval_expr(&call_expr.callee));
        let args = eval_exprs!(self, Self::eval_expr, call_expr.arguments);

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

            debug!("tail_call: {tail_call:?}");

            let fn_hir_id = match tail_call.callee {
                TlangValue::Object(obj) => match self.get_object_by_id(obj) {
                    TlangObjectKind::Fn(hir_id) => *hir_id,
                    TlangObjectKind::NativeFn => {
                        self.panic(format!(
                            "`{:?}` is a native function, cannot tail call",
                            self.state.stringify(tail_call.callee)
                        ));
                    }
                    _ => self.panic(format!(
                        "`{:?}` is not a function",
                        self.state.stringify(tail_call.callee)
                    )),
                },
                _ => self.panic(format!(
                    "`{:?}` is not a function",
                    self.state.stringify(tail_call.callee)
                )),
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
            self.replace_current_fn_scope(&fn_decl, tail_call.callee, &tail_call.args);
            match self.eval_block_inner(&fn_decl.body) {
                EvalResult::TailCall => {}
                result => return result,
            }
        }
    }

    fn replace_current_fn_scope(
        &mut self,
        fn_decl: &Rc<hir::FunctionDeclaration>,
        callee: TlangValue,
        args: &[TlangValue],
    ) {
        debug!("replace_current_fn_scope: {:?}", fn_decl.name());

        self.state
            .current_call_frame_mut()
            .replace_fn_decl(fn_decl.clone());
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
        let applied_args = eval_exprs!(
            self,
            |this: &mut Self, expr: &hir::Expr| {
                if expr.is_wildcard() {
                    EvalResult::Value(TlangValue::Nil)
                } else {
                    this.eval_expr(expr)
                }
            },
            call_expr.arguments
        );

        let fn_object = self.create_native_fn("anonymous", move |_, args| {
            let mut applied_args = applied_args.clone();
            // For each arg in args, replace a hole (Nil) from the already applied args
            for arg in args {
                let index = applied_args
                    .iter()
                    .position(|a| *a == TlangValue::Nil)
                    .unwrap();

                applied_args[index] = *arg;
            }

            NativeFnReturn::CallObject(Box::new((callee, applied_args)))
        });

        EvalResult::Value(fn_object)
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> EvalResult {
        debug!("eval_call: {call_expr:?}");

        if call_expr.has_wildcard() {
            return self.eval_partial_call(call_expr);
        }

        let return_value = match &call_expr.callee.kind {
            hir::ExprKind::Path(path) if let Some(value) = self.resolve_value(path) => {
                let args = eval_exprs!(self, Self::eval_expr, call_expr.arguments);
                self.eval_call_object(value, &args)
            }
            hir::ExprKind::Path(path) if path.res.is_struct_def() => {
                let Some(struct_decl) = self.state.get_struct_decl(path) else {
                    self.panic(format!(
                        "Struct `{}` not found\nCurrent scope: {:?}",
                        path,
                        self.state.current_scope()
                    ));
                };

                eval_value!(self.eval_struct_ctor(call_expr, &struct_decl))
            }
            hir::ExprKind::Path(path) if path.res.is_enum_variant_def() => {
                let Some(enum_decl) = self.state.get_enum_decl(&path.as_init()) else {
                    self.panic(format!(
                        "Enum variant `{}` not found\nCurrent scope: {:?}",
                        path,
                        self.state.current_scope()
                    ));
                };

                eval_value!(self.eval_enum_ctor(call_expr, &enum_decl, path.last_ident()))
            }
            hir::ExprKind::Path(path)
                if let Some(struct_decl) = self.state.get_struct_decl(&path.as_init()) =>
            {
                let args = eval_exprs!(self, Self::eval_expr, call_expr.arguments);
                self.call_shape_method(struct_decl.hir_id.into(), path.last_ident(), &args)
            }
            hir::ExprKind::Path(path)
                if let Some(enum_decl) = self.state.get_enum_decl(&path.as_init()) =>
            {
                eval_value!(self.eval_enum_ctor(call_expr, &enum_decl, path.last_ident()))
            }
            hir::ExprKind::Path(path) => {
                self.panic(format!(
                    "Function `{}` not found\nCurrent scope: {}",
                    path,
                    self.state.debug_stringify_scope_stack()
                ));
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let call_target = eval_value!(self.eval_expr(expr));
                let mut args = eval_exprs!(
                    self,
                    Self::eval_expr,
                    call_expr.arguments,
                    call_expr.arguments.len() + 1
                );
                args.insert(0, call_target);

                let shape_key = self.get_object(call_target).and_then(|o| o.shape());

                if let Some(shape_key) = shape_key {
                    self.call_shape_method(shape_key, ident, &args)
                } else {
                    self.panic(format!(
                        "Field access on non-struct: {:?},\nExpr: {:?}, Current scope: {}",
                        self.state.stringify(call_target),
                        expr,
                        self.state.debug_stringify_scope_stack()
                    ));
                }
            }
            _ => {
                let callee = eval_value!(self.eval_expr(&call_expr.callee));
                let args = eval_exprs!(self, Self::eval_expr, call_expr.arguments);
                self.eval_call_object(callee, &args)
            }
        };

        EvalResult::Value(return_value)
    }

    fn eval_fn_call(
        &mut self,
        fn_decl: &Rc<hir::FunctionDeclaration>,
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

        self.with_new_fn_scope(fn_decl, |this| {
            this.push_value(callee);

            for arg in args {
                this.push_value(*arg);
            }

            // Initialize variable index counter after function parameters (callee + args)
            let param_count = 1 + args.len(); // callee + arguments
            this.state.init_var_index_after_params(param_count);

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
            this.state
                .call_native_fn(id, args)
                .unwrap_or_else(|| this.panic(format!("Native function not found: {id:?}")))
        });

        match r {
            NativeFnReturn::Return(value) => value,
            NativeFnReturn::DynamicCall(id) => {
                if let Some(fn_decl) = self.state.get_fn_decl(id) {
                    self.with_root_scope(|this| this.eval_fn_call(&fn_decl, callee, args))
                        .unwrap_value()
                } else {
                    self.panic(format!("Function not found: {id:?}"));
                }
            }
            NativeFnReturn::CallObject(box (fn_object, args)) => {
                self.eval_call_object(fn_object, &args)
            }
        }
    }

    fn eval_struct_ctor(
        &mut self,
        call_expr: &hir::CallExpression,
        struct_decl: &hir::StructDeclaration,
    ) -> EvalResult {
        // Structs are always instantiated with a single argument, which is a dict.
        let dict_map: HashMap<String, TlangValue> = match &call_expr.arguments[0].kind {
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
        };

        let field_values = struct_decl
            .fields
            .iter()
            .map(|field| dict_map.get(field.name.as_str()).copied().unwrap())
            .collect();

        EvalResult::Value(
            self.state
                .new_object(TlangObjectKind::Struct(TlangStruct::new(
                    struct_decl.hir_id.into(),
                    field_values,
                ))),
        )
    }

    fn eval_enum_ctor(
        &mut self,
        call_expr: &hir::CallExpression,
        enum_decl: &hir::EnumDeclaration,
        variant_name: &Ident,
    ) -> EvalResult {
        // Struct calls are always calls with a single argument, which is a dict.
        // There is a special case for enum variant construction, as they might have
        // multiple arguments which are not wrapped in a dict but are struct values.
        // Currently the only way to distinguish this, is by checking whether the struct
        // definition is a struct with incremental numeric field names, which might cause
        // problems in case someone defines a struct with numeric fields themselves.
        let enum_variant_decl = enum_decl
            .variants
            .iter()
            .find(|variant| variant.name == *variant_name)
            .unwrap_or_else(|| {
                self.panic(format!(
                    "Enum variant `{}` not found in enum `{}`",
                    variant_name, enum_decl.name
                ))
            });

        let is_enum_variant_without_field_names = enum_variant_decl
            .parameters
            .iter()
            .enumerate()
            .all(|(i, field)| field.name.as_str() == i.to_string());

        let dict_map: HashMap<String, TlangValue> = if is_enum_variant_without_field_names {
            // eval_exprs! will allocate a new Vec, we might want to avoid this.
            eval_exprs!(self, Self::eval_expr, call_expr.arguments)
                .into_iter()
                .enumerate()
                .map(|(i, arg)| (i.to_string(), arg))
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

        let field_values = enum_variant_decl
            .parameters
            .iter()
            .map(|field| dict_map.get(&field.name.to_string()).copied().unwrap())
            .collect();

        EvalResult::Value(
            self.state.new_object(TlangObjectKind::Enum(TlangEnum::new(
                enum_decl.hir_id.into(),
                enum_decl
                    .variants
                    .iter()
                    .position(|v| v.name == *variant_name)
                    .unwrap(),
                field_values,
            ))),
        )
    }

    fn call_shape_method(
        &mut self,
        shape_key: ShapeKey,
        method_name: &Ident,
        args: &[TlangValue],
    ) -> TlangValue {
        let shape = self.state.get_shape_by_key(shape_key).unwrap();

        match shape.get_method(method_name.as_str()) {
            Some(TlangStructMethod::HirId(id)) => {
                let fn_decl = self.state.get_fn_decl(*id).unwrap();
                self.with_root_scope(|this| {
                    // TODO: Struct methods should have a value to refer to.
                    this.eval_fn_call(&fn_decl, TlangValue::Nil, args)
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
                    shape.name(),
                    method_name.as_str(),
                    shape.get_method_names(),
                ));
            }
        }
    }

    fn eval_let_stmt(&mut self, pat: &hir::Pat, expr: &hir::Expr, _ty: &hir::Ty) -> EvalResult {
        let value = eval_value!(self.eval_expr(expr));

        if !self.eval_pat(pat, value) {
            self.panic(format!(
                "Pattern did not match value {:?}",
                self.state.stringify(value)
            ));
        }

        EvalResult::Void
    }

    fn eval_list_expr(&mut self, values: &[hir::Expr]) -> EvalResult {
        let mut field_values = Vec::with_capacity(values.len());

        for (i, expr) in values.iter().enumerate() {
            if let hir::ExprKind::Unary(UnaryOp::Spread, expr) = &expr.kind {
                let value = eval_value!(self.eval_expr(expr));

                if let TlangValue::Object(id) = value {
                    match self.get_object_by_id(id) {
                        TlangObjectKind::Slice(slice) => {
                            let values = self.get_slice_values(*slice);
                            field_values.reserve(values.len());
                            field_values.extend_from_slice(values);
                        }
                        TlangObjectKind::Struct(list_struct) => {
                            field_values.reserve(list_struct.len());
                            field_values.extend(list_struct.values());
                        }
                        obj => self.panic(format!("Expected list, got {obj:?}")),
                    }

                    // In case we used all the capacity due to spreading the values above,
                    // we once again reserve the remaining capacity.
                    field_values.reserve(values.len() - i);
                } else {
                    self.panic(format!("Expected list, got {value:?}"));
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
            }
        }

        let shape = ShapeKey::from_dict_keys(&shape_keys);

        if !self.state.has_shape(shape) {
            self.state
                .define_struct_shape(shape, "Dict".to_string(), shape_keys, HashMap::new());
        }

        EvalResult::Value(
            self.state
                .new_object(TlangObjectKind::Struct(TlangStruct::new(
                    shape,
                    field_values,
                ))),
        )
    }

    fn eval_match(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) -> EvalResult {
        let value = eval_value!(self.eval_expr(expr));

        debug!("eval_match: {}", self.state.stringify(value));

        for arm in arms {
            if let MatchResult::Matched(result) = self.eval_match_arm(arm, value) {
                return result;
            }
        }

        EvalResult::Void
    }

    /// Evaluates a match arm and returns the value if it matches, otherwise returns None.
    fn eval_match_arm(&mut self, arm: &hir::MatchArm, value: TlangValue) -> MatchResult {
        debug!(
            "eval_match_arm: {:?} {:?} {}",
            arm.pat.kind,
            arm.guard,
            self.state.stringify(value)
        );

        self.with_new_scope(arm, |this| {
            if !this.eval_pat(&arm.pat, value) {
                return MatchResult::NotMatched(EvalResult::Void);
            }

            if let Some(expr) = &arm.guard {
                if let hir::ExprKind::Let(pat, expr) = &expr.kind {
                    let value = eval_match_value!(this.eval_expr(expr));

                    if !this.eval_pat(pat, value) {
                        return MatchResult::NotMatched(EvalResult::Void);
                    }
                } else if !eval_match_value!(this.eval_expr(expr)).is_truthy(&this.state) {
                    return MatchResult::NotMatched(EvalResult::Void);
                }
            }

            MatchResult::Matched(this.eval_block_inner(&arm.block))
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

                if let (TlangValue::Object(lhs), box token::Literal::String(box rhs)) =
                    (value, literal)
                    && let TlangObjectKind::String(lhs) = self.get_object_by_id(lhs)
                {
                    return *lhs == rhs;
                }

                false
            }
            hir::PatKind::List(patterns) => self.eval_pat_list(patterns, value),
            hir::PatKind::Identifier(_id, ident) => {
                debug!("eval_pat: {} = {}", ident, self.state.stringify(value));

                // Use slot-based assignment for non-global scopes that have allocated slots
                // If scope has 0 locals, use sequential assignment instead
                if self.state.is_global_scope() || !self.state.current_scope_has_slots() {
                    self.push_value(value);
                } else {
                    let _index = self.state.set_let_binding(value);
                }

                true
            }
            hir::PatKind::Wildcard => true,
            hir::PatKind::Enum(path, kvs) if let Some(tlang_enum) = self.get_enum(value) => {
                let variant_index = tlang_enum.variant;
                let shape = self
                    .get_shape_of(value)
                    .unwrap_or_else(|| {
                        self.panic(format!(
                            "Enum shape not found for value {:?}",
                            self.state.stringify(value)
                        ))
                    })
                    .get_enum_shape()
                    .unwrap_or_else(|| {
                        self.panic(format!(
                            "Value has a shape, but not an enum shape {:?}",
                            self.state.stringify(value)
                        ))
                    });
                let path_name = path.as_init().to_string();

                if shape.name != path_name {
                    debug!("eval_pat: Not matched as {} != {}", shape.name, path_name);

                    return false;
                }

                let variant_name = path.last_ident().as_str();
                let variant_shape = &shape.variants[variant_index];

                if variant_shape.name != variant_name {
                    debug!(
                        "eval_pat: Not matched as {} != {}",
                        shape.variants[variant_index].name, variant_name
                    );

                    return false;
                }

                kvs.iter().all(|(k, pat)| {
                    self.get_shape_of(value)
                        .and_then(|shape| shape.get_enum_shape())
                        .map(|shape| &shape.variants[variant_index])
                        .and_then(|variant| variant.get_field_index(&k.to_string()))
                        .is_some_and(|field_index| {
                            let tlang_enum = self.get_enum(value).unwrap();
                            self.eval_pat(pat, tlang_enum.field_values[field_index])
                        })
                })
            }
            hir::PatKind::Enum(path, kvs) => {
                todo!(
                    "eval_pat: Enum({:?}, {:?}) for value {}\nCurrent scope: {}",
                    path,
                    kvs,
                    self.state.stringify(value),
                    self.state.debug_stringify_scope_stack()
                )
            }
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
                        let rest_object = self.state.new_slice(value, i, list_struct.len() - i);

                        return self.eval_pat(pat, rest_object);
                    }

                    if !self.eval_pat(pat, list_struct[i]) {
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

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;
    use indoc::indoc;
    use pretty_assertions::{assert_eq, assert_matches};
    use tlang_hir_opt::HirOptimizer;
    use tlang_span::Span;

    #[ctor::ctor]
    fn before_all() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Warn)
            .parse_default_env()
            .is_test(true)
            .try_init();
    }

    struct TestInterpreter {
        node_id_allocator: tlang_span::NodeIdAllocator,
        semantic_analyzer: tlang_semantics::SemanticAnalyzer,
        interpreter: Interpreter,
        last_span: Span,
    }

    impl TestInterpreter {
        fn new() -> Self {
            let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
            semantic_analyzer.add_builtin_symbols(&Interpreter::builtin_symbols());
            let interpreter = Interpreter::new();

            TestInterpreter {
                node_id_allocator: tlang_span::NodeIdAllocator::default(),
                semantic_analyzer,
                interpreter,
                last_span: Span::default(),
            }
        }

        fn parse_src(&mut self, src: &str) -> tlang_ast::node::Module {
            let mut parser = tlang_parser::Parser::from_source(src)
                .with_line_offset(self.last_span.end.line + 1)
                .set_node_id_allocator(self.node_id_allocator);
            let module = parser.parse().unwrap();

            self.last_span = module.span;
            self.node_id_allocator = *parser.node_id_allocator();

            module
        }

        fn analyze(&mut self, module: &tlang_ast::node::Module) {
            self.semantic_analyzer
                .analyze_as_root_module(module)
                .unwrap();
        }

        fn lower(&mut self, module: &tlang_ast::node::Module) -> hir::Module {
            let mut lowering_context = tlang_ast_lowering::LoweringContext::new(
                self.semantic_analyzer.symbol_id_allocator(),
                self.semantic_analyzer.root_symbol_table(),
                self.semantic_analyzer.symbol_tables().clone(),
            );
            let (mut module, meta) = tlang_ast_lowering::lower(&mut lowering_context, module);

            let mut optimizer = HirOptimizer::default();
            debug!("LowerResultMeta = {:?}", meta);
            optimizer.optimize_hir(&mut module, meta.into());

            module
        }

        fn parse(&mut self, src: &str) -> hir::Module {
            let ast = self.parse_src(src);
            self.analyze(&ast);
            self.lower(&ast)
        }

        fn eval_root(&mut self, src: &str) -> TlangValue {
            let hir = self.parse(src);
            self.interpreter.eval(&hir)
        }

        fn eval(&mut self, src: &str) -> TlangValue {
            let block = format!("{{ {src} }};");
            let hir = self.parse(&block);

            match &hir.block.stmts[0].kind {
                hir::StmtKind::Expr(expr) => self.interpreter.eval_expr(expr).unwrap_value(),
                _ => todo!("eval: {:?}", hir),
            }
        }

        fn state(&self) -> &InterpreterState {
            self.interpreter.state()
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
    fn test_eval_expr_direct() {
        // Test the public eval_expr method directly
        let mut test_interp = TestInterpreter::new();
        let hir = test_interp.parse("{ 1 + 2 };");

        // Extract the block expression from the statement
        if let hir::StmtKind::Expr(expr) = &hir.block.stmts[0].kind {
            if let hir::ExprKind::Block(block_expr) = &expr.kind {
                // Evaluate the expression inside the block
                if let Some(tail_expr) = &block_expr.expr {
                    let result = test_interp.interpreter.eval_expr(tail_expr);
                    assert_eq!(result.unwrap_value(), TlangValue::U64(3));
                }
            }
        }
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

        for (src, expected_value) in &tests {
            match (interpreter.eval(src), expected_value) {
                (TlangValue::Bool(actual), TlangValue::Bool(expected)) => {
                    assert_eq!(actual, *expected);
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

        interpreter
            .interpreter
            .define_native_fn("log", move |_, args| {
                calls_tracker.borrow_mut().push(args.to_vec());
                NativeFnReturn::Return(TlangValue::Nil)
            });

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

        assert_matches!(
            [
                interpreter.eval("Values::One"),
                interpreter.eval("Values::Two"),
                interpreter.eval("Values::Three")
            ],
            [TlangValue::U64(0), TlangValue::U64(1), TlangValue::U64(2)]
        );
    }

    #[test]
    fn test_tagged_enum_declaration_and_use() {
        let mut interpreter = interpreter(indoc! {"
            enum MyOption {
                None,
                Some(Int),
            }
            let some = MyOption::Some(10);
            let none = MyOption::None;
        "});

        let some_value = interpreter.eval("some");
        assert_matches!(some_value, TlangValue::Object(_));

        let none_value = interpreter.eval("none");
        assert_matches!(none_value, TlangValue::Object(_));

        let some_data = match some_value {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_enum()
                .unwrap(),
            val => panic!("Expected struct, got {val:?}"),
        };

        let none_data = match none_value {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_enum()
                .unwrap(),
            val => panic!("Expected struct, got {val:?}"),
        };

        assert_eq!(none_data.variant, 0);
        assert_eq!(some_data.variant, 1);

        assert!(none_data.field_values.is_empty());
        assert_matches!(some_data.field_values[0], TlangValue::U64(10));
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
            val => panic!("Expected slice, got {val:?}"),
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
            val => panic!("Expected slice, got {val:?}"),
        };
        assert_eq!(list_data.start(), 4);
        assert_eq!(list_data.len(), 1);

        let head = interpreter.eval("head(as_slice([1, 2, 3, 4, 5]))");
        assert_eq!(head.as_usize(), 3);
    }

    #[test]
    fn test_nested_function_call() {
        let mut interpreter = interpreter(indoc! {"
            fn foo() {
                fn bar() { 1 }
            }
        "});

        assert_matches!(interpreter.eval("foo()()"), TlangValue::U64(1));
    }

    #[test]
    fn test_simple_loop() {
        let mut interpreter = interpreter(indoc! {"
            fn loop_test() {
                let i = 0;
                loop {
                    if i >= 10 {
                        break;
                    }

                    i = i + 1;
                }
                i
            }
        "});
        assert_matches!(interpreter.eval("loop_test()"), TlangValue::U64(10));
    }

    #[test]
    fn test_for_loop_on_list_simple() {
        let mut interpreter = interpreter(indoc! {"
            fn for_test() {
                let sum = 0;
                for i in [1, 2, 3, 4, 5] {
                    sum = sum + i;
                }
                sum
            }
        "});
        assert_matches!(interpreter.eval("for_test()"), TlangValue::U64(15));
    }

    #[test]
    fn test_for_loop_on_list_with_accumulator() {
        let mut interpreter = interpreter(indoc! {"
            fn for_test() {
                for i in [1, 2, 3, 4, 5] with sum = 0 {
                    sum + i
                }
            }
        "});
        assert_matches!(interpreter.eval("for_test()"), TlangValue::U64(15));
    }

    #[test]
    fn test_for_loop_on_list_with_accumulator_pat() {
        let mut interpreter = interpreter(indoc! {"
            fn even_odd() {
                for n in [1, 2, 3, 4] with [even, odd] = [[], []]; {
                    if n % 2 == 0 {
                        [[...even, n], odd]
                    } else {
                        [even, [...odd, n]]
                    }
                }
            }
        "});

        let result = interpreter.eval("even_odd()");

        assert_eq!(interpreter.state().stringify(result), "[[2, 4], [1, 3]]");
    }
}
