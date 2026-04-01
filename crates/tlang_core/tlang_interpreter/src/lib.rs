#![feature(box_patterns)]
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use smallvec::SmallVec;
use tlang_ast::node::{Ident, UnaryOp, Visibility};
use tlang_ast::token;
use tlang_hir::{self as hir, BindingKind};
use tlang_memory::prelude::*;
use tlang_memory::shape::{ProtocolId, ShapeKey, Shaped, TlangEnumVariant, TlangShape};
use tlang_memory::value::TlangArithmetic;
use tlang_memory::value::object::TlangEnum;
use tlang_memory::{NativeFnReturn, Resolver, VMState, execution, scope};

pub use tlang_memory::NativeFnDef;
pub use tlang_memory::{
    NativeEnumDef, NativeEnumVariantDef, NativeMethodDef, NativeProtocolDef, NativeProtocolImplDef,
    NativeStructDef,
};

#[cfg(feature = "stdlib")]
pub fn init_stdlib(_state: &mut VMState) {
    tlang_stdlib::init();
}

#[cfg(not(feature = "stdlib"))]
pub fn init_stdlib(_state: &mut VMState) {}

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

pub struct Interpreter;

/// A minimal `HirScope` that represents the arm's pattern-variable scope.
/// Used to pre-allocate slots for pattern-bound variables before entering the block scope.
struct ArmPatScope(usize);

impl hir::HirScope for ArmPatScope {
    fn locals(&self) -> usize {
        self.0
    }

    fn upvars(&self) -> usize {
        0
    }

    fn set_locals(&mut self, _: usize) {}

    fn set_upvars(&mut self, _: usize) {}
}

impl Interpreter {
    /// Call handler registered on [`VMState`] so that native functions
    /// can invoke user-defined callables via [`VMState::call`].
    pub fn call_handler(
        state: &mut VMState,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> TlangValue {
        Interpreter.eval_call_object(state, callee, args)
    }

    fn get_shape_of<'a>(&self, state: &'a VMState, value: TlangValue) -> Option<&'a TlangShape> {
        match state.get_object(value)?.shape() {
            Some(s) => state.get_shape_by_key(s),
            _ => state.panic(format!("Cannot get shape of non-struct object: {value:?}")),
        }
    }

    #[inline(always)]
    fn with_new_scope<T, F, R>(&self, state: &mut VMState, meta: &T, f: F) -> R
    where
        T: hir::HirScope,
        F: FnOnce(&Interpreter, &mut VMState) -> R,
    {
        state.enter_scope(meta);
        let result = f(self, state);
        state.exit_scope();
        result
    }

    #[inline(always)]
    fn with_new_fn_scope<F, R>(
        &self,
        state: &mut VMState,
        fn_decl: &Rc<hir::FunctionDeclaration>,
        f: F,
    ) -> R
    where
        F: FnOnce(&Interpreter, &mut VMState) -> R,
    {
        state.push_call_stack(execution::CallStackEntry::new_call(fn_decl));
        let result = self.with_new_scope(state, fn_decl, f);
        state.pop_call_stack();
        result
    }

    #[inline(always)]
    fn with_root_scope<F, R>(&self, state: &mut VMState, f: F) -> R
    where
        F: FnOnce(&Interpreter, &mut VMState) -> R,
    {
        let root_scope = vec![*state.execution.scope_stack.root_scope()];
        self.with_scope(state, root_scope, f)
    }

    /// Invoke a closure by pushing `[root, capture_scope]` instead of
    /// restoring the full saved scope chain.
    ///
    /// The capture scope contains the captured values in the order defined
    /// by `CaptureInfo`.  After `FreeVariableAnalysis` has remapped the
    /// Upvar slots, `Upvar(cap_idx, block_depth + 1)` resolves to
    /// `capture_scope[cap_idx]`.
    ///
    /// `capture_positions` are the original memory positions for each
    /// captured variable.  They are stored on the capture scope so that
    /// `capture_position` resolves through to the ultimate original binding,
    /// preserving correct two-way sync for nested closures.
    ///
    /// Returns `(body_result, modified_captures)` — the caller must write
    /// the modified captures back to the `TlangClosure` to persist mutations.
    #[inline(always)]
    fn with_closure_scope<F, R>(
        &self,
        state: &mut VMState,
        captures: &[TlangValue],
        capture_positions: &[Option<scope::CapturePosition>],
        f: F,
    ) -> (R, scope::CaptureVec)
    where
        F: FnOnce(&Interpreter, &mut VMState) -> R,
    {
        let root_scope = vec![*state.execution.scope_stack.root_scope()];
        let old_scopes = std::mem::replace(&mut state.execution.scope_stack.scopes, root_scope);
        // Also save and clear capture_origin since we're replacing scopes.
        let old_origin = state.execution.scope_stack.take_capture_origin();
        let cap_start = state
            .execution
            .scope_stack
            .push_capture_scope(captures, Some(capture_positions));
        let result = f(self, state);
        // Read back modified captures before restoring scopes so mutations
        // to captured variables persist across invocations.
        let modified = state
            .execution
            .scope_stack
            .read_back_captures(cap_start, captures.len());
        // Truncate the memory buffer back to where it was before the capture
        // scope was pushed.  All capture-scope and closure-body memory is
        // temporary; we've already read back the modified captures above.
        // Without this truncation, every closure invocation permanently grows
        // the memory buffer.
        state.execution.scope_stack.truncate_memory(cap_start);
        state.execution.scope_stack.scopes = old_scopes;
        state
            .execution
            .scope_stack
            .restore_capture_origin(old_origin);
        (result, modified)
    }

    #[inline(always)]
    fn with_scope<F, R>(&self, state: &mut VMState, scopes: Vec<scope::Scope>, f: F) -> R
    where
        F: FnOnce(&Interpreter, &mut VMState) -> R,
    {
        let old_scopes = std::mem::replace(&mut state.execution.scope_stack.scopes, scopes);
        let result = f(self, state);
        state.execution.scope_stack.scopes = old_scopes;
        result
    }

    pub fn eval(&self, state: &mut VMState, input: &hir::Module) -> TlangValue {
        self.eval_block_inner(state, &input.block).unwrap_value()
    }

    /// Evaluate a module in its own scope frame.
    ///
    /// Unlike [`eval`], which evaluates directly in the current (global) scope,
    /// this method creates a new scope for the module block. This prevents
    /// scope-slot collisions when multiple modules are evaluated sequentially
    /// (e.g. in multi-module projects).
    pub fn eval_module(&self, state: &mut VMState, input: &hir::Module) -> TlangValue {
        self.with_new_scope(state, input, |this, state| {
            this.eval_block_inner(state, &input.block).unwrap_value()
        })
    }

    #[inline(always)]
    fn eval_block_inner(&self, state: &mut VMState, block: &hir::Block) -> EvalResult {
        propagate!(self.eval_stmts(state, &block.stmts));

        self.eval_block_expr(state, block)
    }

    #[inline(always)]
    fn eval_block_expr(&self, state: &mut VMState, block: &hir::Block) -> EvalResult {
        if let Some(expr) = &block.expr {
            self.eval_expr(state, expr)
        } else {
            EvalResult::Void
        }
    }

    fn eval_block(&self, state: &mut VMState, block: &hir::Block) -> EvalResult {
        self.with_new_scope(state, block, |this, state| {
            this.eval_block_inner(state, block)
        })
    }

    fn eval_loop(&self, state: &mut VMState, block: &hir::Block) -> EvalResult {
        loop {
            match self.eval_block(state, block) {
                EvalResult::Break(value) => return EvalResult::Value(value),
                EvalResult::Return(value) => return EvalResult::Return(value),
                EvalResult::TailCall => return EvalResult::TailCall,
                EvalResult::Continue | EvalResult::Value(_) | EvalResult::Void => {}
            }
        }
    }

    fn eval_stmt(&self, state: &mut VMState, stmt: &hir::Stmt) -> EvalResult {
        state.set_current_span(stmt.span);

        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.eval_expr(state, expr),
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.eval_fn_decl(state, decl);
                EvalResult::Void
            }
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.eval_dyn_fn_decl(state, decl);
                EvalResult::Void
            }
            hir::StmtKind::StructDeclaration(decl) => {
                self.eval_struct_decl(state, decl);
                EvalResult::Void
            }
            hir::StmtKind::EnumDeclaration(decl) => {
                self.eval_enum_decl(state, decl);
                EvalResult::Void
            }
            hir::StmtKind::Let(pat, expr, ty) => self.eval_let_stmt(state, pat, expr, ty),
            hir::StmtKind::Const(_, pat, expr, ty) => self.eval_let_stmt(state, pat, expr, ty),
            hir::StmtKind::ProtocolDeclaration(decl) => {
                self.eval_protocol_decl(state, decl);
                EvalResult::Void
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                self.eval_impl_block(state, impl_block);
                EvalResult::Void
            }
            hir::StmtKind::Return(Some(expr)) => {
                EvalResult::Return(eval_value!(state, self.eval_expr(state, expr)))
            }

            hir::StmtKind::Return(_) => EvalResult::Return(TlangValue::Nil),
        }
    }

    fn eval_stmts(&self, state: &mut VMState, stmts: &[hir::Stmt]) -> EvalResult {
        for stmt in stmts {
            propagate!(self.eval_stmt(state, stmt));
        }

        EvalResult::Void
    }

    /// Evaluate an expression and return the result.
    /// This is useful for benchmarking and testing individual expressions.
    pub fn eval_expr(&self, state: &mut VMState, expr: &hir::Expr) -> EvalResult {
        state.set_current_span(expr.span);

        // Return cached constant pool value if available.
        if state.is_constant_pool_expr(expr.hir_id)
            && let Some(cached) = state.get_constant(expr.hir_id)
        {
            return EvalResult::Value(cached);
        }

        let result = self.eval_expr_inner(state, expr);

        // Cache the result if this expression is in the constant pool.
        if state.is_constant_pool_expr(expr.hir_id)
            && let EvalResult::Value(value) = result
        {
            state.set_constant(expr.hir_id, value);
        }

        result
    }

    fn eval_expr_inner(&self, state: &mut VMState, expr: &hir::Expr) -> EvalResult {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                EvalResult::Value(state.resolve_value(path).unwrap_or_else(|| {
                    let scope_debug = state.debug_stringify_scope_stack();
                    state.panic(format!(
                        "Could not resolve path \"{}\" ({:?})\nCurrent scope: {}",
                        path, path.res, scope_debug
                    ))
                }))
            }
            hir::ExprKind::Literal(value) => EvalResult::Value(self.eval_literal(state, value)),
            hir::ExprKind::List(values) => self.eval_list_expr(state, values),
            hir::ExprKind::Dict(entries) => self.eval_dict_expr(state, entries),
            hir::ExprKind::IndexAccess(lhs, rhs) => self.eval_index_access(state, lhs, rhs),
            hir::ExprKind::FieldAccess(lhs, rhs) => self.eval_field_access(state, lhs, rhs),
            hir::ExprKind::Block(block) => self.eval_block(state, block),
            hir::ExprKind::Loop(block) => self.eval_loop(state, block),
            hir::ExprKind::Break(Some(expr)) => {
                EvalResult::Break(eval_value!(state, self.eval_expr(state, expr)))
            }
            hir::ExprKind::Break(_) => EvalResult::Break(TlangValue::Nil),
            hir::ExprKind::Continue => EvalResult::Continue,
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(state, *op, lhs, rhs),
            hir::ExprKind::Call(call_expr) => self.eval_call(state, call_expr),
            hir::ExprKind::TailCall(call_expr) => self.eval_tail_call(state, call_expr),
            hir::ExprKind::Cast(_expr, _ty) => todo!("eval_expr: Cast"),
            hir::ExprKind::Unary(op, expr) => self.eval_unary(state, *op, expr),
            hir::ExprKind::IfElse(condition, consequence, else_clauses) => {
                self.eval_if_else(state, condition, consequence, else_clauses)
            }
            hir::ExprKind::FunctionExpression(fn_decl) => {
                EvalResult::Value(state.new_closure(fn_decl))
            }
            hir::ExprKind::Match(expr, arms) => self.eval_match(state, expr, arms),
            hir::ExprKind::Range(..) => todo!("eval_expr: Range"),
            hir::ExprKind::TaggedString { tag, parts, exprs } => {
                let tag_fn = eval_value!(state, self.eval_expr(state, tag));
                let parts_values: Vec<TlangValue> = parts
                    .iter()
                    .map(|p| {
                        let s = state.new_string(p.to_string());
                        state.push_temp_root(s);
                        s
                    })
                    .collect();
                let parts_list = state.new_list(parts_values);
                state.push_temp_root(parts_list);
                let mut value_list = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    value_list.push(eval_value!(state, self.eval_expr(state, expr)));
                }
                let values_list = state.new_list(value_list);
                EvalResult::Value(self.eval_call_object(state, tag_fn, &[parts_list, values_list]))
            }
            hir::ExprKind::Let(..) => state.panic(
                "Let expressions are only valid in match guards and if expressions".to_string(),
            ),
            hir::ExprKind::Wildcard => state.panic("Wildcard not allowed here".to_string()),
        }
    }

    fn eval_if_else(
        &self,
        state: &mut VMState,
        condition: &hir::Expr,
        consequence: &hir::Block,
        else_clauses: &[hir::ElseClause],
    ) -> EvalResult {
        let value = eval_value!(state, self.eval_expr(state, condition));

        if state.is_truthy(value) {
            return self.eval_block(state, consequence);
        }

        for else_clause in else_clauses {
            if let Some(condition) = &else_clause.condition {
                let value = eval_value!(state, self.eval_expr(state, condition));

                if state.is_truthy(value) {
                    return self.eval_block(state, &else_clause.consequence);
                }
            } else {
                return self.eval_block(state, &else_clause.consequence);
            }
        }

        EvalResult::Void
    }

    fn eval_index_access(
        &self,
        state: &mut VMState,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
    ) -> EvalResult {
        let rhs_value = eval_value!(state, self.eval_expr(state, rhs));
        let lhs_value = eval_value!(state, self.eval_expr(state, lhs));

        match state.get_object(lhs_value) {
            Some(TlangObjectKind::Struct(obj)) => EvalResult::Value(obj[rhs_value.as_usize()]),
            Some(TlangObjectKind::Slice(slice)) => {
                EvalResult::Value(state.get_slice_value(*slice, rhs_value.as_usize()))
            }
            _ => todo!("eval_index_access: {:?}[{:?}]", lhs, rhs_value),
        }
    }

    fn eval_field_access(&self, state: &mut VMState, lhs: &hir::Expr, ident: &Ident) -> EvalResult {
        let value = eval_value!(state, self.eval_expr(state, lhs));

        if let Some(TlangObjectKind::Struct(obj)) = state.get_object(value) {
            if let Some(index) = state.get_struct_field_index(obj.shape(), ident.as_str()) {
                return EvalResult::Value(obj[index]);
            }

            let shape = state
                .get_shape(obj)
                .and_then(|shape| shape.get_struct_shape())
                .unwrap();

            state.panic(format!(
                "Could not find field `{}` on {}",
                ident, shape.name
            ));
        }

        if value.is_nil() {
            state.panic(format!("Cannot access field `{ident}` on nil"));
        }

        if !value.is_object() {
            let s = state.stringify(value);
            state.panic(format!(
                "Cannot access field `{ident}` on non-object: {}",
                s
            ));
        }

        let s = state.stringify(value);
        todo!("eval_field_access: {}.{}", s, ident);
    }

    fn eval_literal(&self, state: &mut VMState, literal: &token::Literal) -> TlangValue {
        match literal {
            token::Literal::Integer(value) => TlangValue::I64(*value),
            token::Literal::UnsignedInteger(value) => TlangValue::U64(*value),
            token::Literal::Float(value) => TlangValue::F64(*value),
            token::Literal::Boolean(value) => TlangValue::Bool(*value),
            token::Literal::String(id) => state.new_string(tlang_intern::get(*id).to_string()),
            token::Literal::Char(id) => state.new_string(tlang_intern::get(*id).to_string()),
            token::Literal::None => unreachable!(),
        }
    }

    fn eval_unary(&self, state: &mut VMState, op: UnaryOp, expr: &hir::Expr) -> EvalResult {
        match op {
            UnaryOp::Not => {
                let value = eval_value!(state, self.eval_expr(state, expr));

                EvalResult::Value(TlangValue::Bool(!state.is_truthy(value)))
            }
            UnaryOp::BitwiseNot => {
                let value = eval_value!(state, self.eval_expr(state, expr));

                EvalResult::Value(match value.as_primitive() {
                    TlangPrimitive::Int(v) => TlangValue::I64(!v),
                    TlangPrimitive::UInt(v) => TlangValue::I64(!(v as i64)),
                    _ => todo!("eval_unary BitwiseNot: incompatible type {:?}", value),
                })
            }
            UnaryOp::Rest => unreachable!("Rest operator implemented in eval_list_expr"),
            _ => todo!("eval_unary: {:?}", op),
        }
    }

    fn eval_binary(
        &self,
        state: &mut VMState,
        op: hir::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
    ) -> EvalResult {
        match op {
            hir::BinaryOpKind::And => {
                let lhs = eval_value!(state, self.eval_expr(state, lhs));

                if state.is_truthy(lhs) {
                    let rhs = eval_value!(state, self.eval_expr(state, rhs));

                    debug!(
                        "eval_binary: {:?} && {:?}",
                        state.stringify(lhs),
                        state.stringify(rhs)
                    );

                    if state.is_truthy(rhs) {
                        return EvalResult::Value(TlangValue::Bool(true));
                    }
                }

                debug!("eval_binary: {:?} && ...", state.stringify(lhs));

                return EvalResult::Value(TlangValue::Bool(false));
            }

            hir::BinaryOpKind::Or => {
                let lhs = eval_value!(state, self.eval_expr(state, lhs));

                if state.is_truthy(lhs) {
                    debug!("eval_binary: {:?} || ...", state.stringify(lhs));

                    return EvalResult::Value(TlangValue::Bool(true));
                }

                return self.eval_expr(state, rhs);
            }

            hir::BinaryOpKind::Assign if let hir::ExprKind::Path(path) = &lhs.kind => {
                let value = eval_value!(state, self.eval_expr(state, rhs));

                debug!("eval_binary: {} = {}", path, state.stringify(value));

                state.execution.scope_stack.update_value(&path.res, value);

                return EvalResult::Value(value);
            }

            hir::BinaryOpKind::Assign
                if let hir::ExprKind::FieldAccess(base, ident) = &lhs.kind =>
            {
                let struct_value = eval_value!(state, self.eval_expr(state, base));
                let struct_shape = state
                    .get_object(struct_value)
                    .and_then(|o| o.shape())
                    .unwrap_or_else(|| {
                        state.panic(format!("Cannot assign to field `{ident}` on non-object"))
                    });
                let index = state
                    .get_struct_field_index(struct_shape, ident.as_str())
                    .unwrap_or_else(|| {
                        state.panic(format!(
                            "Cannot assign to field `{ident}` on struct `{struct_shape:?}`"
                        ))
                    });

                let value = eval_value!(state, self.eval_expr(state, rhs));

                let struct_obj = state.get_struct_mut(struct_value).unwrap();

                struct_obj[index] = value;

                return EvalResult::Value(value);
            }

            hir::BinaryOpKind::Assign => {
                todo!("eval_binary: Assign not implemented for {:?}", lhs);
            }

            _ => {}
        }

        let lhs = eval_value!(state, self.eval_expr(state, lhs));
        let rhs = eval_value!(state, self.eval_expr(state, rhs));

        debug!(
            "eval_binary: {:?} {:?} {:?}",
            state.stringify(lhs),
            op,
            state.stringify(rhs)
        );

        if let TlangValue::Object(_) = lhs {
            return self.eval_object_binary_op(state, op, lhs, rhs);
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

            hir::BinaryOpKind::BitwiseAnd => {
                self.eval_bitwise_op(lhs, rhs, |a, b| a & b, |a, b| a & b)
            }
            hir::BinaryOpKind::BitwiseOr => {
                self.eval_bitwise_op(lhs, rhs, |a, b| a | b, |a, b| a | b)
            }
            hir::BinaryOpKind::BitwiseXor => {
                self.eval_bitwise_op(lhs, rhs, |a, b| a ^ b, |a, b| a ^ b)
            }

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
        &self,
        state: &mut VMState,
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

                        let obj_lhs = state.get_object_by_id(lhs_id).unwrap();
                        let obj_rhs = state.get_object_by_id(rhs_id).unwrap();

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
                                state.stringify(lhs),
                                state.stringify(rhs)
                            ),
                        }
                    }
                    _ => todo!(
                        "eval_object_binary_op: {:?}, {:?}, {:?}",
                        op,
                        state.stringify(lhs),
                        state.stringify(rhs)
                    ),
                }
            }
            hir::BinaryOpKind::Add => match (lhs, rhs) {
                (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                    let obj_lhs = state.get_object_by_id(lhs_id).unwrap();
                    let obj_rhs = state.get_object_by_id(rhs_id).unwrap();

                    match (obj_lhs, obj_rhs) {
                        (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                            EvalResult::Value(state.new_string(lhs.clone() + rhs))
                        }
                        _ => todo!(
                            "eval_object_binary_op: {:?}, {:?}, {:?}",
                            op,
                            state.stringify(lhs),
                            state.stringify(rhs)
                        ),
                    }
                }
                _ => todo!(
                    "eval_object_binary_op: {:?}, {:?}, {:?}",
                    op,
                    state.stringify(lhs),
                    state.stringify(rhs)
                ),
            },
            _ => todo!(
                "eval_object_binary_op: {:?}, {:?}, {:?}",
                op,
                state.stringify(lhs),
                state.stringify(rhs)
            ),
        }
    }

    fn eval_bitwise_op<F, G>(
        &self,
        lhs: TlangValue,
        rhs: TlangValue,
        int_op: F,
        uint_op: G,
    ) -> TlangValue
    where
        F: Fn(i64, i64) -> i64,
        G: Fn(u64, u64) -> u64,
    {
        match (lhs.as_primitive(), rhs.as_primitive()) {
            (TlangPrimitive::Int(l), TlangPrimitive::Int(r)) => TlangValue::I64(int_op(l, r)),
            (TlangPrimitive::UInt(l), TlangPrimitive::UInt(r)) => TlangValue::U64(uint_op(l, r)),
            _ => todo!("eval_bitwise_op: incompatible types"),
        }
    }

    fn eval_fn_decl(&self, state: &mut VMState, decl: &hir::FunctionDeclaration) {
        state.set_fn_decl(decl.hir_id, Rc::new(decl.clone()));

        let fn_object = state.new_object(TlangObjectKind::Fn(decl.hir_id));

        if state.is_global_scope() || !state.current_scope_has_slots() {
            state.execution.scope_stack.push_value(fn_object);
        } else {
            state.set_let_binding(fn_object);
        }

        match &decl.name.kind {
            hir::ExprKind::Path(path) => {
                // Used for static struct method resolution, for now..
                state.set_global(path.to_string(), fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_fn_decl: {:?}", expr),
                };

                // Used for static struct method resolution, for now..
                state.set_global(path.to_string() + ident.as_str(), fn_object);

                match &path.res.binding_kind() {
                    BindingKind::Struct => {
                        let struct_decl = state.get_struct_decl(path).unwrap();

                        state.set_struct_method(
                            struct_decl.hir_id.into(),
                            ident.as_str(),
                            TlangStructMethod::HirId(decl.hir_id),
                        );
                    }
                    BindingKind::Enum => {
                        let enum_decl = state.get_enum_decl(path).unwrap();

                        state.set_enum_method(
                            enum_decl.hir_id.into(),
                            ident.as_str(),
                            TlangStructMethod::HirId(decl.hir_id),
                        );
                    }
                    BindingKind::Unknown => {
                        state.panic(format!(
                            "Could not define method {ident} on unresolved path: {path:?}"
                        ));
                    }
                    _ => todo!("eval_fn_decl: {:?}", path),
                }
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn create_dyn_fn_object(
        &self,
        state: &mut VMState,
        decl: &hir::DynFunctionDeclaration,
    ) -> TlangValue {
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

        state.new_native_fn(&(name.clone() + "/*"), move |state, args| {
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

    fn eval_dyn_fn_decl(&self, state: &mut VMState, decl: &hir::DynFunctionDeclaration) {
        let dyn_fn_object = self.create_dyn_fn_object(state, decl);

        if state.is_global_scope() || !state.current_scope_has_slots() {
            state.execution.scope_stack.push_value(dyn_fn_object);
        } else {
            state.set_let_binding(dyn_fn_object);
        }

        match &decl.name.kind {
            hir::ExprKind::Path(path) => {
                // Used for static struct method resolution, for now..
                state.set_global(path.to_string(), dyn_fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_dyn_fn_decl: {:?}", expr),
                };
                let struct_decl = state.get_struct_decl(path).unwrap();
                state.set_struct_method(
                    struct_decl.hir_id.into(),
                    ident.as_str(),
                    TlangStructMethod::Native(dyn_fn_object.get_object_id().unwrap()),
                );
            }
            _ => todo!("eval_dyn_fn_decl: {:?}", decl.name),
        }
    }

    fn eval_struct_decl(&self, state: &mut VMState, decl: &hir::StructDeclaration) {
        state.set_struct_decl(decl.name.to_string(), Rc::new(decl.clone()));

        let struct_shape = TlangShape::new_struct_shape(
            decl.name.to_string(),
            decl.fields
                .iter()
                .map(|field| field.name.to_string())
                .collect(),
            HashMap::new(),
        );

        state.set_shape(decl.hir_id.into(), struct_shape);
    }

    fn eval_enum_decl(&self, state: &mut VMState, decl: &hir::EnumDeclaration) {
        state.set_enum_decl(decl.name.to_string(), Rc::new(decl.clone()));

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
            for (index, variant) in decl.variants.iter().enumerate() {
                let v = if decl.is_discriminant_enum() {
                    // Safe: is_discriminant_enum() guarantees all variants have discriminants.
                    let discriminant = variant.discriminant.as_deref().unwrap();
                    self.eval_expr(state, discriminant).unwrap_value()
                } else {
                    TlangValue::from(index)
                };
                if state.is_global_scope() || !state.current_scope_has_slots() {
                    state.execution.scope_stack.push_value(v);
                } else {
                    state.set_let_binding(v);
                }
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
                let enum_value = state.new_object(TlangObjectKind::Enum(TlangEnum::new(
                    shape_key,
                    variant_index,
                    vec![],
                )));

                if state.is_global_scope() || !state.current_scope_has_slots() {
                    state.execution.scope_stack.push_value(enum_value);
                } else {
                    state.set_let_binding(enum_value);
                }
            }

            let enum_shape =
                TlangShape::new_enum_shape(decl.name.to_string(), variant_shapes, HashMap::new());

            state.set_shape(shape_key, enum_shape);
        }
    }

    fn eval_protocol_decl(&self, state: &mut VMState, decl: &hir::ProtocolDeclaration) {
        let protocol_id = ProtocolId::Hir(decl.hir_id);
        let protocol_name = decl.name.to_string();
        state.register_protocol(
            protocol_id,
            protocol_name,
            decl.methods.iter().map(|m| m.name.to_string()).collect(),
        );

        // Register default implementations for methods that have bodies
        for method in &decl.methods {
            if let Some(body) = &method.body {
                let method_name = method.name.to_string();

                // Build a FunctionDeclaration from the protocol method signature
                let name_path =
                    hir::Path::new(vec![hir::PathSegment::new(method.name)], method.span);
                let name_expr = hir::Expr {
                    hir_id: method.hir_id,
                    kind: hir::ExprKind::Path(Box::new(name_path)),
                    ty: hir::Ty::unknown(),
                    span: method.span,
                };
                let fn_decl = hir::FunctionDeclaration {
                    hir_id: method.hir_id,
                    visibility: Visibility::Private,
                    name: name_expr,
                    parameters: method.parameters.clone(),
                    return_type: method.return_type.clone(),
                    body: body.clone(),
                    span: method.span,
                };

                state.set_fn_decl(method.hir_id, Rc::new(fn_decl));
                let fn_value = state.new_object(TlangObjectKind::Fn(method.hir_id));
                state.register_protocol_impl(
                    protocol_id,
                    ShapeKey::Wildcard,
                    &method_name,
                    fn_value,
                );
            }
        }
    }

    fn eval_impl_block(&self, state: &mut VMState, impl_block: &hir::ImplBlock) {
        let protocol_id = impl_block
            .protocol_name
            .res
            .hir_id()
            .map(ProtocolId::Hir)
            .or_else(|| {
                // Fallback to name-based lookup for unresolved paths
                state.protocol_id_by_name(&impl_block.protocol_name.to_string())
            })
            .unwrap_or_else(|| {
                state.panic(format!(
                    "Protocol `{}` not found (unresolved in HIR and not registered in runtime)",
                    impl_block.protocol_name
                ))
            });

        let target_type_shape_key = impl_block
            .target_type
            .res
            .hir_id()
            .map(ShapeKey::from)
            .or_else(|| {
                // Fallback to name-based shape lookup for native types
                state.lookup_builtin_shape(&impl_block.target_type.to_string())
            })
            .unwrap_or_else(|| {
                state.panic(format!(
                    "Type `{}` not found for protocol implementation",
                    impl_block.target_type
                ))
            });

        for method in &impl_block.methods {
            // Register the function declaration so it can be called
            state.set_fn_decl(method.hir_id, Rc::new(method.clone()));
            let fn_value = state.new_object(TlangObjectKind::Fn(method.hir_id));

            let method_name = match &method.name.kind {
                hir::ExprKind::Path(path) => path.last_ident().to_string(),
                hir::ExprKind::FieldAccess(_, ident) => ident.to_string(),
                _ => unreachable!(),
            };
            state.register_protocol_impl(
                protocol_id,
                target_type_shape_key,
                &method_name,
                fn_value,
            );
        }

        for apply_ident in &impl_block.apply_methods {
            let method_name = apply_ident.as_str();
            self.install_protocol_method_on_shape(
                state,
                protocol_id,
                target_type_shape_key,
                &impl_block.target_type.to_string(),
                method_name,
            );
        }
    }

    fn is_builtin_type(type_name: &str) -> bool {
        matches!(type_name, "List" | "Option" | "Result" | "ListIterator")
    }

    fn install_protocol_method_on_shape(
        &self,
        state: &mut VMState,
        protocol_id: ProtocolId,
        target_type: ShapeKey,
        target_type_name: &str,
        method_name: &str,
    ) {
        if Self::is_builtin_type(target_type_name) {
            state.panic(format!(
                "Cannot use 'apply' for built-in type '{target_type_name}': \
                 applying methods to built-in types is not allowed to preserve backwards compatibility"
            ));
        }

        let shape_key = target_type;

        // Collision check
        if state
            .heap
            .get_shape_by_key(shape_key)
            .and_then(|s| s.get_method(method_name))
            .is_some()
        {
            state.panic(format!(
                "Method collision: '{method_name}' is already defined on '{target_type_name}'"
            ));
        }

        let fn_value = state
            .get_protocol_impl(protocol_id, Some(target_type), method_name)
            .unwrap_or_else(|| {
                state.panic(format!(
                    "Cannot install method '{method_name}': not found in impl for '{target_type_name}'"
                ))
            });

        let obj_id = fn_value.get_object_id().unwrap_or_else(|| {
            state.panic(format!(
                "Cannot install method '{method_name}': protocol impl is not a function object"
            ))
        });
        let method = match state.get_object_by_id(obj_id).unwrap() {
            TlangObjectKind::Fn(hir_id) => TlangStructMethod::HirId(*hir_id),
            _ => state.panic(format!(
                "Cannot install method '{method_name}': protocol impl is not a Fn object"
            )),
        };
        state.heap.set_method(shape_key, method_name, method);
    }

    fn eval_call_object(
        &self,
        state: &mut VMState,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> TlangValue {
        let id = callee
            .get_object_id()
            .unwrap_or_else(|| state.panic(format!("`{callee:?}` is not a function")));

        match state.get_object_by_id(id).unwrap() {
            TlangObjectKind::Closure(closure) => {
                let closure_decl = state.get_closure_decl(closure.id).unwrap();
                let stale_captures = closure.captures.clone();
                let capture_positions = closure.capture_positions.clone();

                // Fresh-read from original memory positions so the closure sees
                // mutations made by the enclosing scope since it was created.
                let captures: scope::CaptureVec = stale_captures
                    .iter()
                    .zip(capture_positions.iter())
                    .map(|(&stale, pos)| match pos {
                        Some(scope::CapturePosition::Local(p)) => state
                            .execution
                            .scope_stack
                            .read_memory_at(*p)
                            .unwrap_or(stale),
                        Some(scope::CapturePosition::Global(p)) => state
                            .execution
                            .scope_stack
                            .read_global_at(*p)
                            .unwrap_or(stale),
                        None => stale,
                    })
                    .collect();

                let (result, modified_captures) =
                    self.with_closure_scope(state, &captures, &capture_positions, |this, state| {
                        this.eval_fn_call(state, &closure_decl, callee, args)
                            .unwrap_value()
                    });

                // Write back to original memory positions so the enclosing
                // scope sees mutations made by the closure (two-way sync).
                for (pos, &value) in capture_positions.iter().zip(modified_captures.iter()) {
                    match pos {
                        Some(scope::CapturePosition::Local(p)) => {
                            state.execution.scope_stack.write_memory_at(*p, value);
                        }
                        Some(scope::CapturePosition::Global(p)) => {
                            state.execution.scope_stack.write_global_at(*p, value);
                        }
                        None => {}
                    }
                }

                // Update closure's captures for the next invocation.
                if let Some(TlangObjectKind::Closure(closure)) = state.get_object_by_id_mut(id) {
                    closure.captures = modified_captures;
                }

                result
            }
            TlangObjectKind::Fn(hir_id) => {
                let fn_decl = state
                    .get_fn_decl(*hir_id)
                    .unwrap_or_else(|| state.panic("Function not found".to_string()));

                self.with_root_scope(state, |this, state| {
                    this.eval_fn_call(state, &fn_decl, callee, args)
                        .unwrap_value()
                })
            }
            TlangObjectKind::NativeFn => self.exec_native_fn(state, id, callee, args),
            obj => state.panic(format!("`{obj:?}` is not a function")),
        }
    }

    fn eval_tail_call(&self, state: &mut VMState, call_expr: &hir::CallExpression) -> EvalResult {
        if call_expr.has_wildcard() {
            state.panic("Tail call with wildcard arguments not allowed".to_string());
        }

        let callee = eval_value!(state, self.eval_expr(state, &call_expr.callee));
        let args = eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments);

        state
            .current_call_frame_mut()
            .set_tail_call(execution::TailCall { callee, args });

        EvalResult::TailCall
    }

    fn tail_call(&self, state: &mut VMState) -> EvalResult {
        loop {
            let tail_call = state.current_call_frame_mut().tail_call.take().unwrap();

            debug!("tail_call: {tail_call:?}");

            let fn_hir_id = match tail_call.callee {
                TlangValue::Object(obj) => match state.get_object_by_id(obj).unwrap() {
                    TlangObjectKind::Fn(hir_id) => *hir_id,
                    TlangObjectKind::NativeFn => {
                        let s = state.stringify(tail_call.callee);
                        state.panic(format!("`{:?}` is a native function, cannot tail call", s));
                    }
                    _ => {
                        let s = state.stringify(tail_call.callee);
                        state.panic(format!("`{:?}` is not a function", s))
                    }
                },
                _ => {
                    let s = state.stringify(tail_call.callee);
                    state.panic(format!("`{:?}` is not a function", s))
                }
            };

            // Optimized for self referencial tail calls, if we are calling the same function,
            // we'll reuse the fn declaration stored on the current call frame.
            let fn_decl = match state.current_call_frame().get_fn_decl() {
                Some(fn_decl) if fn_decl.hir_id == fn_hir_id => fn_decl.clone(),
                _ => state.get_fn_decl(fn_hir_id).unwrap_or_else(|| {
                    state.panic(format!("Function `{:?}` not found", tail_call.callee));
                }),
            };

            // Instead of a recursive call, replace the current function scope
            self.replace_current_fn_scope(state, &fn_decl, tail_call.callee, &tail_call.args);
            match self.eval_block_inner(state, &fn_decl.body) {
                EvalResult::TailCall => {}
                result => return result,
            }
        }
    }

    fn replace_current_fn_scope(
        &self,
        state: &mut VMState,
        fn_decl: &Rc<hir::FunctionDeclaration>,
        callee: TlangValue,
        args: &[TlangValue],
    ) {
        debug!("replace_current_fn_scope: {:?}", fn_decl.name());

        state
            .current_call_frame_mut()
            .replace_fn_decl(fn_decl.clone());
        state.execution.scope_stack.clear_current_scope();

        // TODO: Methods currently do not reserve a slot for the fn itself.
        if fn_decl.name.path().is_some() {
            state.execution.scope_stack.push_value(callee);
        }

        for arg in args {
            state.execution.scope_stack.push_value(*arg);
        }
    }

    fn eval_partial_call(
        &self,
        state: &mut VMState,
        call_expr: &hir::CallExpression,
    ) -> EvalResult {
        let callee = eval_value!(state, self.eval_expr(state, &call_expr.callee));
        let applied_args = eval_exprs!(
            state,
            |s: &mut VMState, expr: &hir::Expr| {
                if expr.is_wildcard() {
                    EvalResult::Value(TlangValue::Nil)
                } else {
                    self.eval_expr(s, expr)
                }
            },
            call_expr.arguments
        );

        let fn_object = state.new_native_fn("anonymous", move |_, args| {
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

    #[allow(clippy::too_many_lines)]
    fn eval_call(&self, state: &mut VMState, call_expr: &hir::CallExpression) -> EvalResult {
        debug!("eval_call: {call_expr:?}");

        if call_expr.has_wildcard() {
            return self.eval_partial_call(state, call_expr);
        }

        let mark = state.temp_roots_mark();

        let return_value = match &call_expr.callee.kind {
            hir::ExprKind::Path(path) if let Some(value) = state.resolve_value(path) => {
                let args = eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments);
                self.eval_call_object(state, value, &args)
            }
            hir::ExprKind::Path(path) if path.res.is_struct_def() => {
                let Some(struct_decl) = state.get_struct_decl(path) else {
                    state.panic(format!(
                        "Struct `{}` not found\nCurrent scope: {:?}",
                        path,
                        state.current_scope()
                    ));
                };

                eval_value!(state, self.eval_struct_ctor(state, call_expr, &struct_decl))
            }
            hir::ExprKind::Path(path) if path.res.is_enum_variant_def() => {
                let Some(enum_decl) = state.get_enum_decl(&path.as_init()) else {
                    state.panic(format!(
                        "Enum variant `{}` not found\nCurrent scope: {:?}",
                        path,
                        state.current_scope()
                    ));
                };

                eval_value!(
                    state,
                    self.eval_enum_ctor(state, call_expr, &enum_decl, path.last_ident())
                )
            }
            hir::ExprKind::Path(path)
                if let Some(struct_decl) = state.get_struct_decl(&path.as_init()) =>
            {
                let args = eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments);
                self.call_shape_method(
                    state,
                    struct_decl.hir_id.into(),
                    path.last_ident().as_str(),
                    &args,
                )
            }
            hir::ExprKind::Path(path)
                if let Some(enum_decl) = state.get_enum_decl(&path.as_init()) =>
            {
                eval_value!(
                    state,
                    self.eval_enum_ctor(state, call_expr, &enum_decl, path.last_ident())
                )
            }
            hir::ExprKind::Path(path)
                if path.segments.len() == 2
                    && state.is_protocol(path.segments[0].ident.as_str()) =>
            {
                let protocol_name = path.segments[0].ident.as_str();
                let method_name = path.segments[1].ident.as_str();
                let protocol_id = state.protocol_id_by_name(protocol_name).unwrap();
                let args = eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments);
                let type_shape_key = state.type_shape_key_of(args[0]);
                let type_name = state.type_name_of(args[0]);
                let Some(fn_value) =
                    state.get_protocol_impl(protocol_id, type_shape_key, method_name)
                else {
                    state.panic(format!(
                        "No implementation of `{protocol_name}::{method_name}` for type `{type_name}`"
                    ));
                };
                self.eval_call_object(state, fn_value, &args)
            }
            hir::ExprKind::Path(path) => {
                let scope_debug = state.debug_stringify_scope_stack();
                state.panic(format!(
                    "Function `{}` not found\nCurrent scope: {}",
                    path, scope_debug
                ));
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let call_target = eval_value!(state, self.eval_expr(state, expr));
                let mut args = eval_exprs!(
                    state,
                    |s, e| self.eval_expr(s, e),
                    call_expr.arguments,
                    call_expr.arguments.len() + 1
                );
                args.insert(0, call_target);

                let shape_key = state.get_object(call_target).and_then(|o| o.shape());

                if let Some(shape_key) = shape_key {
                    self.call_shape_method(state, shape_key, ident.as_str(), &args)
                } else {
                    let s = state.stringify(call_target);
                    let scope_debug = state.debug_stringify_scope_stack();
                    state.panic(format!(
                        "Field access on non-struct: {:?},\nExpr: {:?}, Current scope: {}",
                        s, expr, scope_debug
                    ));
                }
            }
            _ => {
                let callee = eval_value!(state, self.eval_expr(state, &call_expr.callee));
                let args = eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments);
                self.eval_call_object(state, callee, &args)
            }
        };

        state.temp_roots_restore(mark);

        EvalResult::Value(return_value)
    }

    fn eval_fn_call(
        &self,
        state: &mut VMState,
        fn_decl: &Rc<hir::FunctionDeclaration>,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> EvalResult {
        debug!(
            "eval_fn_call: {} {:?} with args {:?}",
            fn_decl.name(),
            state.stringify(callee),
            args.iter().map(|a| state.stringify(*a)).collect::<Vec<_>>()
        );

        if fn_decl.parameters.len() != args.len() {
            state.panic(format!(
                "Function `{:?}` expects {} arguments, but got {}",
                fn_decl.name(),
                fn_decl.parameters.len(),
                args.len()
            ));
        }

        self.with_new_fn_scope(state, fn_decl, |this, state| {
            // Slots are pre-allocated by ScopeStack::push; use indexed assignment so
            // that nested calls (which start their scope at memory.len()) don't
            // overlap with this scope's parameter/local slots.
            state.execution.scope_stack.set_local(0, callee);

            for (i, arg) in args.iter().enumerate() {
                state.execution.scope_stack.set_local(i + 1, *arg);
            }

            // Initialize variable index counter after function parameters (callee + args)
            let param_count = 1 + args.len(); // callee + arguments
            state.init_var_index_after_params(param_count);

            match this.eval_block_inner(state, &fn_decl.body) {
                EvalResult::TailCall => this.tail_call(state),
                result => result,
            }
        })
    }

    fn exec_native_fn(
        &self,
        state: &mut VMState,
        id: TlangObjectId,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> TlangValue {
        // Do we need and want to reset the scope for native functions?
        // It could be somewhat interesting to manipulate the current scope from native functions.
        let r = self.with_root_scope(state, |_this, state| {
            state
                .call_native_fn(id, args)
                .unwrap_or_else(|| state.panic(format!("Native function not found: {id:?}")))
        });

        match r {
            NativeFnReturn::Return(value) => value,
            NativeFnReturn::DynamicCall(id) => {
                if let Some(fn_decl) = state.get_fn_decl(id) {
                    self.with_root_scope(state, |this, state| {
                        this.eval_fn_call(state, &fn_decl, callee, args)
                    })
                    .unwrap_value()
                } else {
                    state.panic(format!("Function not found: {id:?}"));
                }
            }
            NativeFnReturn::CallObject(box (fn_object, args)) => {
                self.eval_call_object(state, fn_object, &args)
            }
        }
    }

    fn eval_struct_ctor(
        &self,
        state: &mut VMState,
        call_expr: &hir::CallExpression,
        struct_decl: &hir::StructDeclaration,
    ) -> EvalResult {
        // Structs are always instantiated with a single argument, which is a dict.
        let dict_map: HashMap<String, TlangValue> = match &call_expr.arguments[0].kind {
            hir::ExprKind::Dict(entries) => {
                let mut map = HashMap::with_capacity(entries.len());
                for (key_expr, value_expr) in entries {
                    let key = match &key_expr.kind {
                        hir::ExprKind::Path(path) => path.first_ident().to_string(),
                        _ => todo!("eval_call: {:?}", key_expr),
                    };
                    let value = eval_value!(state, self.eval_expr(state, value_expr));
                    map.insert(key, value);
                }
                map
            }
            _ => todo!("eval_call: {:?}", call_expr.arguments[0]),
        };

        let field_values = struct_decl
            .fields
            .iter()
            .map(|field| dict_map.get(field.name.as_str()).copied().unwrap())
            .collect();

        EvalResult::Value(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
            struct_decl.hir_id.into(),
            field_values,
        ))))
    }

    fn eval_enum_ctor(
        &self,
        state: &mut VMState,
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
                state.panic(format!(
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
            eval_exprs!(state, |s, e| self.eval_expr(s, e), call_expr.arguments)
                .into_iter()
                .enumerate()
                .map(|(i, arg)| (i.to_string(), arg))
                .collect()
        } else {
            match &call_expr.arguments[0].kind {
                hir::ExprKind::Dict(entries) => {
                    let mut map = HashMap::with_capacity(entries.len());
                    for (key_expr, value_expr) in entries {
                        let key = match &key_expr.kind {
                            hir::ExprKind::Path(path) => path.first_ident().to_string(),
                            _ => todo!("eval_call: {:?}", key_expr),
                        };
                        let value = eval_value!(state, self.eval_expr(state, value_expr));
                        map.insert(key, value);
                    }
                    map
                }
                _ => todo!("eval_call: {:?}", call_expr.arguments[0]),
            }
        };

        let field_values = enum_variant_decl
            .parameters
            .iter()
            .map(|field| dict_map.get(&field.name.to_string()).copied().unwrap())
            .collect();

        EvalResult::Value(
            state.new_object(TlangObjectKind::Enum(TlangEnum::new(
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
        &self,
        state: &mut VMState,
        shape_key: ShapeKey,
        method_name: &str,
        args: &[TlangValue],
    ) -> TlangValue {
        let shape = state.get_shape_by_key(shape_key).unwrap();

        match shape.get_method(method_name) {
            Some(TlangStructMethod::HirId(id)) => {
                let fn_decl = state.get_fn_decl(*id).unwrap();
                self.with_root_scope(state, |this, state| {
                    // TODO: Struct methods should have a value to refer to.
                    this.eval_fn_call(state, &fn_decl, TlangValue::Nil, args)
                        .unwrap_value()
                })
            }
            Some(TlangStructMethod::Native(id)) => {
                // TODO: Struct methods should have a value to refer to.
                self.exec_native_fn(state, *id, TlangValue::Nil, args)
            }
            _ => {
                state.panic(format!(
                    "{} does not have a method {:?}, {:?}",
                    shape.name(),
                    method_name,
                    shape.get_method_names(),
                ));
            }
        }
    }

    fn eval_let_stmt(
        &self,
        state: &mut VMState,
        pat: &hir::Pat,
        expr: &hir::Expr,
        _ty: &hir::Ty,
    ) -> EvalResult {
        let value = eval_value!(state, self.eval_expr(state, expr));

        if !self.eval_pat(state, pat, value) {
            let s = state.stringify(value);
            state.panic(format!("Pattern did not match value {:?}", s));
        }

        EvalResult::Void
    }

    fn eval_list_expr(&self, state: &mut VMState, values: &[hir::Expr]) -> EvalResult {
        let mut field_values = Vec::with_capacity(values.len());

        for (i, expr) in values.iter().enumerate() {
            if let hir::ExprKind::Unary(UnaryOp::Spread, expr) = &expr.kind {
                let value = eval_value!(state, self.eval_expr(state, expr));

                if let TlangValue::Object(id) = value {
                    match state.get_object_by_id(id).unwrap() {
                        TlangObjectKind::Slice(slice) => {
                            let values = state.get_slice_values(*slice);
                            field_values.reserve(values.len());
                            field_values.extend_from_slice(values);
                        }
                        TlangObjectKind::Struct(list_struct) => {
                            field_values.reserve(list_struct.len());
                            field_values.extend(list_struct.values());
                        }
                        _ => {
                            let list = self.collect_iterable(state, value);
                            let list_struct = state.get_struct(list).unwrap();
                            field_values.reserve(list_struct.len());
                            field_values.extend(list_struct.values());
                        }
                    }

                    // In case we used all the capacity due to spreading the values above,
                    // we once again reserve the remaining capacity.
                    field_values.reserve(values.len() - i);
                } else {
                    state.panic(format!("Expected list, got {value:?}"));
                }
            } else {
                field_values.push(eval_value!(state, self.eval_expr(state, expr)));
            }
        }

        let list = state.new_list(field_values);

        EvalResult::Value(list)
    }

    /// Collects an iterable value into a list by calling `Iterable::iter` then
    /// draining `Iterator::next` until `Option::None`.
    fn collect_iterable(&self, state: &mut VMState, value: TlangValue) -> TlangValue {
        let mark = state.temp_roots_mark();
        let type_name = state.type_name_of(value).to_string();
        let type_shape_key = state.type_shape_key_of(value);
        let iterable_id = state.protocol_id_by_name("Iterable").unwrap();
        let iter_fn = state
            .get_protocol_impl(iterable_id, type_shape_key, "iter")
            .unwrap_or_else(|| {
                state.panic(format!(
                    "Type `{type_name}` is not iterable (no `Iterable::iter` implementation)"
                ))
            });
        let iterator = self.eval_call_object(state, iter_fn, &[value]);
        state.push_temp_root(iterator);

        let iter_type_name = state.type_name_of(iterator).to_string();
        let iter_type_shape_key = state.type_shape_key_of(iterator);
        let iterator_id = state.protocol_id_by_name("Iterator").unwrap();
        let next_fn = state
            .get_protocol_impl(iterator_id, iter_type_shape_key, "next")
            .unwrap_or_else(|| {
                state.panic(format!(
                    "Iterator type `{iter_type_name}` has no `Iterator::next` implementation"
                ))
            });

        let option_shape = state.heap.builtin_shapes.option;
        let mut items = Vec::new();

        loop {
            let result = self.eval_call_object(state, next_fn, &[iterator]);
            let tlang_enum = state
                .get_enum(result)
                .unwrap_or_else(|| state.panic("Iterator::next must return an Option".to_string()));

            if tlang_enum.shape() != option_shape {
                state.panic("Iterator::next must return an Option".to_string());
            }

            // Option::Some = variant 0, Option::None = variant 1
            if tlang_enum.variant == 1 {
                break;
            }

            items.push(tlang_enum.field_values[0]);
        }

        let result = state.new_list(items);
        state.temp_roots_restore(mark);
        result
    }

    fn eval_dict_expr(
        &self,
        state: &mut VMState,
        entries: &[(hir::Expr, hir::Expr)],
    ) -> EvalResult {
        let mut field_values: Vec<TlangValue> = Vec::with_capacity(entries.len());
        let mut shape_keys = Vec::with_capacity(entries.len());

        for entry in entries {
            field_values.push(eval_value!(state, self.eval_expr(state, &entry.1)));

            // As we primarily had compilation to JS in mind, paths here should actually be
            // strings instead. Need to update the parser to emit strings instead of paths,
            // and paths when using brackets.
            match &entry.0.kind {
                hir::ExprKind::Path(path) => shape_keys.push(path.first_ident().to_string()),
                _ => todo!("eval_dict_expr: {:?}", entry.0),
            }
        }

        let shape = ShapeKey::from_dict_keys(&shape_keys);

        if !state.has_shape(shape) {
            state.define_struct_shape(shape, "Dict".to_string(), shape_keys, HashMap::new());
        }

        EvalResult::Value(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
            shape,
            field_values,
        ))))
    }

    fn eval_match(
        &self,
        state: &mut VMState,
        expr: &hir::Expr,
        arms: &[hir::MatchArm],
    ) -> EvalResult {
        let value = eval_value!(state, self.eval_expr(state, expr));

        debug!("eval_match: {}", state.stringify(value));

        for arm in arms {
            if let MatchResult::Matched(result) = self.eval_match_arm(state, arm, value) {
                return result;
            }
        }

        let s = state.stringify(value);
        state.panic(format!("No match found for value {:?}", s));
    }

    /// Evaluates a match arm and returns the value if it matches, otherwise returns None.
    fn eval_match_arm(
        &self,
        state: &mut VMState,
        arm: &hir::MatchArm,
        value: TlangValue,
    ) -> MatchResult {
        debug!(
            "eval_match_arm: {:?} {:?} {}",
            arm.pat.kind,
            arm.guard,
            state.stringify(value)
        );

        if arm.hir_id != arm.block.hir_id {
            // Block-body arm: push the arm's pattern scope, then evaluate the block
            // (which pushes its own scope). Two scopes in total.
            self.with_new_scope(state, &ArmPatScope(arm.pat_locals), |this, state| {
                if !this.eval_pat(state, &arm.pat, value) {
                    return MatchResult::NotMatched(EvalResult::Void);
                }

                if let Some(expr) = &arm.guard {
                    if let hir::ExprKind::Let(pat, expr) = &expr.kind {
                        let value = eval_match_value!(this.eval_expr(state, expr));

                        if !this.eval_pat(state, pat, value) {
                            return MatchResult::NotMatched(EvalResult::Void);
                        }
                    } else {
                        let value = eval_match_value!(this.eval_expr(state, expr));

                        if !state.is_truthy(value) {
                            return MatchResult::NotMatched(EvalResult::Void);
                        }
                    }
                }

                MatchResult::Matched(this.eval_block(state, &arm.block))
            })
        } else {
            // Inline-expr arm: single scope combining the arm and its expression.
            self.with_new_scope(state, arm, |this, state| {
                if !this.eval_pat(state, &arm.pat, value) {
                    return MatchResult::NotMatched(EvalResult::Void);
                }

                if let Some(expr) = &arm.guard {
                    if let hir::ExprKind::Let(pat, expr) = &expr.kind {
                        let value = eval_match_value!(this.eval_expr(state, expr));

                        if !this.eval_pat(state, pat, value) {
                            return MatchResult::NotMatched(EvalResult::Void);
                        }
                    } else {
                        let value = eval_match_value!(this.eval_expr(state, expr));

                        if !state.is_truthy(value) {
                            return MatchResult::NotMatched(EvalResult::Void);
                        }
                    }
                }

                MatchResult::Matched(this.eval_block_inner(state, &arm.block))
            })
        }
    }

    fn eval_pat(&self, state: &mut VMState, pat: &hir::Pat, value: TlangValue) -> bool {
        state.set_current_span(pat.span);

        match &pat.kind {
            hir::PatKind::Literal(literal) => {
                let literal_value = self.eval_literal(state, literal);

                if value == literal_value {
                    return true;
                }

                if let (TlangValue::Object(lhs), box token::Literal::String(id)) = (value, literal)
                    && let TlangObjectKind::String(lhs) = state.get_object_by_id(lhs).unwrap()
                {
                    return *lhs == tlang_intern::get(*id);
                }

                false
            }
            hir::PatKind::List(patterns) => self.eval_pat_list(state, patterns, value),
            hir::PatKind::Identifier(_id, ident) => {
                debug!("eval_pat: {} = {}", ident, state.stringify(value));

                // Use slot-based assignment for non-global scopes that have allocated slots
                // If scope has 0 locals, use sequential assignment instead
                if state.is_global_scope() || !state.current_scope_has_slots() {
                    state.execution.scope_stack.push_value(value);
                } else {
                    let _index = state.set_let_binding(value);
                }

                true
            }
            hir::PatKind::Wildcard => true,
            hir::PatKind::Enum(path, kvs) if let Some(tlang_enum) = state.get_enum(value) => {
                let variant_index = tlang_enum.variant;
                let shape_opt = self.get_shape_of(state, value);
                let shape = match shape_opt {
                    Some(s) => s,
                    None => {
                        let s = state.stringify(value);
                        state.panic(format!("Enum shape not found for value {:?}", s))
                    }
                };
                let enum_shape = match shape.get_enum_shape() {
                    Some(s) => s,
                    None => {
                        let s = state.stringify(value);
                        state.panic(format!("Value has a shape, but not an enum shape {:?}", s))
                    }
                };
                let path_name = path.as_init().to_string();

                if enum_shape.name != path_name {
                    debug!(
                        "eval_pat: Not matched as {} != {}",
                        enum_shape.name, path_name
                    );

                    return false;
                }

                let variant_name = path.last_ident().as_str();
                let variant_shape = &enum_shape.variants[variant_index];

                if variant_shape.name != variant_name {
                    debug!(
                        "eval_pat: Not matched as {} != {}",
                        enum_shape.variants[variant_index].name, variant_name
                    );

                    return false;
                }

                kvs.iter().all(|(k, pat)| {
                    self.get_shape_of(state, value)
                        .and_then(|shape| shape.get_enum_shape())
                        .map(|shape| &shape.variants[variant_index])
                        .and_then(|variant| variant.get_field_index(&k.to_string()))
                        .is_some_and(|field_index| {
                            let tlang_enum = state.get_enum(value).unwrap();
                            self.eval_pat(state, pat, tlang_enum.field_values[field_index])
                        })
                })
            }
            hir::PatKind::Enum(path, kvs) if state.get_struct(value).is_some() => {
                self.eval_pat_struct(state, path, kvs, value)
            }
            hir::PatKind::Enum(path, kvs) if kvs.is_empty() => {
                // Handle simple enum variants (no parameters) stored as plain values.
                // This covers both auto-indexed enums (variants stored as 0, 1, 2, …)
                // and discriminant enums (variants stored as their explicit value).
                // Previously this case was missing, causing simple enum pattern matching
                // to always return false when the value was not a TlangEnum object.
                state.resolve_value(path) == Some(value)
            }
            hir::PatKind::Enum(_path, _kvs) => false,
            hir::PatKind::Rest(_) => unreachable!("Rest patterns can only appear in list patterns"),
        }
    }

    fn eval_pat_struct(
        &self,
        state: &mut VMState,
        path: &hir::Path,
        kvs: &[(Ident, hir::Pat)],
        value: TlangValue,
    ) -> bool {
        let Some(expected_hir_id) = path.res.hir_id() else {
            return false;
        };

        let value_shape_key = state.get_struct(value).unwrap().shape();

        if value_shape_key != ShapeKey::HirId(expected_hir_id) {
            return false;
        }

        kvs.iter().all(|(k, pat)| {
            state
                .get_shape_by_key(value_shape_key)
                .and_then(|shape| shape.get_struct_shape())
                .and_then(|struct_shape| struct_shape.get_field_index(&k.to_string()))
                .is_some_and(|field_index| {
                    let tlang_struct = state.get_struct(value).unwrap();
                    self.eval_pat(state, pat, tlang_struct[field_index])
                })
        })
    }

    // TODO: Instead of having rest patterns within list patterns, we should have a pattern
    //       specifically for matching against tail values (list, strings, objects)
    fn eval_pat_list(&self, state: &mut VMState, patterns: &[hir::Pat], value: TlangValue) -> bool {
        if !value.is_object() {
            return false;
        }

        match state.get_object(value).unwrap() {
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
                    let list_struct = state.get_struct(value).unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object = state.new_slice(value, i, list_struct.len() - i);
                        state.push_temp_root(rest_object);
                        return self.eval_pat(state, pat, rest_object);
                    }

                    if !self.eval_pat(state, pat, list_struct[i]) {
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
                    let list_slice = state.get_slice(value).unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object = state.new_slice(
                            list_slice.of(),
                            list_slice.start() + i,
                            list_slice.len() - i,
                        );
                        state.push_temp_root(rest_object);
                        return self.eval_pat(state, pat, rest_object);
                    }

                    if !self.eval_pat(state, pat, state.get_slice_value(list_slice, i)) {
                        return false;
                    }
                }

                true
            }
            TlangObjectKind::String(s) => {
                let str_len = s.len();

                // Empty list pattern is a special case, it only matches empty strings.
                if patterns.is_empty() && str_len >= 1 {
                    return false;
                }

                for (i, pat) in patterns.iter().enumerate() {
                    let str_value = state
                        .get_object(value)
                        .and_then(|obj| obj.as_str())
                        .unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object = if i <= str_value.len() {
                            state.new_string(str_value[i..].to_string())
                        } else {
                            state.new_string(String::new())
                        };
                        state.push_temp_root(rest_object);
                        return self.eval_pat(state, pat, rest_object);
                    }

                    let char_match = if let Some(character) = str_value.chars().nth(i) {
                        state.new_string(character.to_string())
                    } else {
                        state.new_string(String::new())
                    };
                    state.push_temp_root(char_match);

                    if !self.eval_pat(state, pat, char_match) {
                        return false;
                    }
                }

                true
            }
            _ => {
                let list = self.collect_iterable(state, value);
                self.eval_pat_list(state, patterns, list)
            }
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
        state: VMState,
        last_span: Span,
    }

    impl TestInterpreter {
        fn new() -> Self {
            let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
            semantic_analyzer.add_builtin_symbols_with_slots(&tlang_vm::VM::builtin_symbols());
            let vm = tlang_vm::VM::new();

            TestInterpreter {
                node_id_allocator: tlang_span::NodeIdAllocator::default(),
                semantic_analyzer,
                state: vm.into_state(),
                last_span: Span::default(),
            }
        }

        fn parse_src(&mut self, src: &str) -> (tlang_ast::node::Module, tlang_parser::ParseMeta) {
            let mut parser = tlang_parser::Parser::from_source(src)
                .with_line_offset(self.last_span.end_lc.line + 1)
                .with_byte_offset(self.last_span.end)
                .set_node_id_allocator(self.node_id_allocator);
            let (module, parse_meta) = parser.parse().unwrap();

            self.last_span = module.span;
            self.node_id_allocator = parse_meta.node_id_allocator;

            (module, parse_meta)
        }

        fn analyze(&mut self, module: &mut tlang_ast::node::Module) {
            self.semantic_analyzer
                .analyze_as_root_module(module)
                .unwrap();
        }

        fn lower(
            &mut self,
            module: &tlang_ast::node::Module,
            parse_meta: &tlang_parser::ParseMeta,
        ) -> hir::Module {
            let mut lowering_context = tlang_ast_lowering::LoweringContext::new(
                self.semantic_analyzer.symbol_id_allocator(),
                self.semantic_analyzer.root_symbol_table(),
                self.semantic_analyzer.symbol_tables().clone(),
            );
            let (mut module, meta) = tlang_ast_lowering::lower(
                &mut lowering_context,
                module,
                &parse_meta.constant_pool_node_ids,
            )
            .expect("lowering should succeed");

            let mut optimizer = HirOptimizer::new(vec![
                Box::new(tlang_hir_opt::TailPositionAnalysis),
                Box::new(tlang_hir_opt::SymbolResolution::default()),
                Box::new(tlang_hir_opt::ConstantFolding::default()),
                // NOTE: DeadCodeElimination is intentionally excluded here.
                // The TestInterpreter evaluates declarations incrementally
                // (REPL-like), so top-level bindings from earlier calls must
                // survive even when unreferenced within the current unit.
                Box::new(tlang_hir_opt::SlotAllocation::default()),
                Box::new(tlang_hir_opt::FreeVariableAnalysis),
            ]);
            debug!("LowerResultMeta = {:?}", meta);
            let constant_pool_ids = meta.constant_pool_ids.clone();
            let mut ctx = meta.into();
            optimizer
                .optimize_hir(&mut module, &mut ctx)
                .expect("internal compiler error: HIR optimizer failed to converge");

            self.state.register_constant_pool_ids(constant_pool_ids);

            module
        }

        fn parse(&mut self, src: &str) -> hir::Module {
            let (mut ast, parse_meta) = self.parse_src(src);
            self.analyze(&mut ast);
            self.lower(&ast, &parse_meta)
        }

        fn eval_root(&mut self, src: &str) -> TlangValue {
            let hir = self.parse(src);
            Interpreter.eval(&mut self.state, &hir)
        }

        fn eval(&mut self, src: &str) -> TlangValue {
            let block = format!("{{ {src} }};");
            let hir = self.parse(&block);

            match &hir.block.stmts[0].kind {
                hir::StmtKind::Expr(expr) => {
                    Interpreter.eval_expr(&mut self.state, expr).unwrap_value()
                }
                _ => todo!("eval: {:?}", hir),
            }
        }

        fn state(&self) -> &VMState {
            &self.state
        }

        fn state_mut(&mut self) -> &mut VMState {
            &mut self.state
        }

        fn object_count(&self) -> usize {
            self.state().object_count()
        }

        fn memory_stats(&self) -> &tlang_memory::MemoryStats {
            self.state().memory_stats()
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
        let hir::StmtKind::Expr(expr) = &hir.block.stmts[0].kind else {
            panic!(
                "Expected Expr statement, found {:?}",
                &hir.block.stmts[0].kind
            );
        };

        let hir::ExprKind::Block(block_expr) = &expr.kind else {
            panic!("Expected Block expression, found {:?}", &expr.kind);
        };

        // Evaluate the expression inside the block
        let Some(tail_expr) = &block_expr.expr else {
            panic!("Expected tail expression in block, found None");
        };

        let result = Interpreter.eval_expr(&mut test_interp.state, tail_expr);
        assert_eq!(result.unwrap_value(), TlangValue::U64(3));
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

        {
            let fn_object = interpreter.state.new_native_fn("log", move |_, args| {
                calls_tracker.borrow_mut().push(args.to_vec());
                NativeFnReturn::Return(TlangValue::Nil)
            });
            debug!("Defining global native function: log");
            interpreter.state.set_global("log".to_string(), fn_object);
        }

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
                .state()
                .get_object_by_id(id)
                .unwrap()
                .as_enum()
                .unwrap(),
            val => panic!("Expected struct, got {val:?}"),
        };

        let none_data = match none_value {
            TlangValue::Object(id) => interpreter
                .state()
                .get_object_by_id(id)
                .unwrap()
                .as_enum()
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
                .state()
                .get_object_by_id(id)
                .unwrap()
                .as_slice()
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
                .state()
                .get_object_by_id(id)
                .unwrap()
                .as_slice()
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

        assert_eq!(
            interpreter.state_mut().stringify(result),
            "[[2, 4], [1, 3]]"
        );
    }

    #[test]
    fn test_stress_gc_struct_ctor_object_fields() {
        let mut t = interpreter(indoc! {"
            struct Named {
                label: Int,
                value: Int,
            }
        "});
        t.state_mut().set_stress_gc(true);
        // Pass string values even though fields are typed Int; types aren't enforced at runtime.
        let result = t.eval(r#"Named { label: "hello", value: "world" }"#);
        // stringify prints strings without quotes; fields are sorted alphabetically.
        assert_eq!(
            t.state_mut().stringify(result),
            "Named { label: hello, value: world }"
        );
    }

    #[test]
    fn test_stress_gc_enum_ctor_object_fields() {
        let mut t = interpreter(indoc! {"
            enum Shape {
                Circle(Int),
                Rect(Int, Int),
            }
        "});
        t.state_mut().set_stress_gc(true);
        // Positional enum variant with string arguments exercises the eval_exprs! path.
        let result = t.eval(r#"Shape::Rect("wide", "tall")"#);
        assert_matches!(result, TlangValue::Object(_));
        let enum_data = match result {
            TlangValue::Object(id) => t.state().get_object_by_id(id).unwrap().as_enum().unwrap(),
            _ => panic!("expected Object"),
        };
        assert_eq!(enum_data.variant, 1);
        let field0 = enum_data.field_values[0];
        let field1 = enum_data.field_values[1];
        assert_eq!(t.state_mut().stringify(field0), "wide");
        assert_eq!(t.state_mut().stringify(field1), "tall");
    }

    #[test]
    fn test_stress_gc_pat_list_rest_slice() {
        let mut t = interpreter(indoc! {"
            fn first_and_rest(list) {
                match list {
                    [first, ...rest] => [first, rest],
                    _ => [],
                }
            }
        "});
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"first_and_rest(["a", "b", "c"])"#);
        // first is a string (no quotes in stringify), rest is a slice (&[...]).
        assert_eq!(t.state_mut().stringify(result), "[a, &[b, c]]");
    }

    #[test]
    fn test_stress_gc_pat_list_rest_string() {
        let mut t = interpreter(indoc! {"
            fn split_first(s) {
                match s {
                    [first, ...rest] => [first, rest],
                    _ => [],
                }
            }
        "});
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"split_first("hello")"#);
        // Both first and rest are strings; stringify prints them without quotes.
        assert_eq!(t.state_mut().stringify(result), "[h, ello]");
    }

    #[test]
    fn test_stress_gc_string_concatenation() {
        let mut t = interpreter("");
        t.state_mut().set_stress_gc(true);
        // String concatenation allocates a new string via new_string.
        let result = t.eval(r#""hello" + " " + "world""#);
        assert_eq!(t.state_mut().stringify(result), "hello world");
    }

    #[test]
    fn test_stress_gc_list_of_strings() {
        let mut t = interpreter("");
        t.state_mut().set_stress_gc(true);
        // Each string literal allocates, then new_list allocates the list.
        let result = t.eval(r#"["a", "b", "c", "d", "e"]"#);
        assert_eq!(t.state_mut().stringify(result), "[a, b, c, d, e]");
    }

    #[test]
    fn test_stress_gc_list_spread() {
        let mut t = interpreter(indoc! {"
            fn prepend(x, list) { [x, ...list] }
        "});
        t.state_mut().set_stress_gc(true);
        // Spread creates a new list from existing objects.
        let result = t.eval(r#"prepend("x", ["a", "b", "c"])"#);
        assert_eq!(t.state_mut().stringify(result), "[x, a, b, c]");
    }

    #[test]
    fn test_stress_gc_recursive_string_decomposition() {
        let mut t = interpreter(indoc! {r#"
            fn reverse(str) { reverse(str, "") }
            fn reverse("", acc) { acc }
            fn reverse([x, ...xs], acc) { rec reverse(xs, x + acc) }
        "#});
        t.state_mut().set_stress_gc(true);
        // Recursive string decomposition creates many char/rest/concat strings.
        let result = t.eval(r#"reverse("abcdef")"#);
        assert_eq!(t.state_mut().stringify(result), "fedcba");
    }

    #[test]
    fn test_stress_gc_closure_captures() {
        let mut t = interpreter(indoc! {r#"
            fn make_greeter(greeting) {
                fn(name) { greeting + " " + name }
            }
        "#});
        t.state_mut().set_stress_gc(true);
        // Closure captures a string, then concatenation inside allocates more strings.
        let result = t.eval(r#"make_greeter("hello")("world")"#);
        assert_eq!(t.state_mut().stringify(result), "hello world");
    }

    #[test]
    fn test_stress_gc_nested_closures() {
        let mut t = interpreter(indoc! {"
            fn compose(f, g) { fn(x) { f(g(x)) } }
            fn add(a) { fn(b) { a + b } }
            fn multiply(a) { fn(b) { a * b } }
        "});
        t.state_mut().set_stress_gc(true);
        // compose creates a closure, and both add/multiply create closures.
        let result = t.eval("compose(multiply(3), add(5))(2)");
        assert_matches!(result, TlangValue::U64(21));
    }

    #[test]
    fn test_stress_gc_recursive_list_construction() {
        let mut t = interpreter(indoc! {r#"
            fn build(0) { [] }
            fn build(n) { ["item", ...build(n - 1)] }
        "#});
        t.state_mut().set_stress_gc(true);
        // Each recursive call creates a string + list + spread.
        let result = t.eval("build(5)");
        assert_eq!(
            t.state_mut().stringify(result),
            "[item, item, item, item, item]"
        );
    }

    #[test]
    fn test_stress_gc_map_filter_with_strings() {
        let mut t = interpreter(indoc! {r#"
            fn map([], _) { [] }
            fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
        "#});
        t.state_mut().set_stress_gc(true);
        // map creates closures, new strings from concatenation, and new lists.
        let result = t.eval(r#"map(["a", "b", "c"], fn(s) { s + s })"#);
        assert_eq!(t.state_mut().stringify(result), "[aa, bb, cc]");
    }

    #[test]
    fn test_stress_gc_for_loop_accumulator_with_objects() {
        let mut t = interpreter(indoc! {r#"
            fn collect_strings() {
                for n in [1, 2, 3, 4] with acc = []; {
                    [...acc, "x"]
                }
            }
        "#});
        t.state_mut().set_stress_gc(true);
        // Each iteration creates a new string and a new list via spread.
        let result = t.eval("collect_strings()");
        assert_eq!(t.state_mut().stringify(result), "[x, x, x, x]");
    }

    #[test]
    fn test_stress_gc_enum_pattern_matching_with_objects() {
        let mut t = interpreter(indoc! {r#"
            enum Wrapper {
                Val(Int),
            }
            fn unwrap(w) {
                match w {
                    Wrapper::Val(v) => v,
                }
            }
        "#});
        t.state_mut().set_stress_gc(true);
        // Enum wrapping and unwrapping with string values.
        let result = t.eval(r#"unwrap(Wrapper::Val("hello"))"#);
        assert_eq!(t.state_mut().stringify(result), "hello");
    }

    #[test]
    fn test_stress_gc_multiple_struct_constructions() {
        let mut t = interpreter(indoc! {"
            struct Pair {
                fst: Int,
                snd: Int,
            }
            fn swap(p) { Pair { fst: p.snd, snd: p.fst } }
        "});
        t.state_mut().set_stress_gc(true);
        // Creates multiple structs in sequence. Fields access existing objects,
        // swap creates a new struct from field values of an existing one.
        let result = t.eval(r#"swap(Pair { fst: "a", snd: "b" })"#);
        assert_eq!(t.state_mut().stringify(result), "Pair { fst: b, snd: a }");
    }

    #[test]
    fn test_stress_gc_chained_function_calls() {
        let mut t = interpreter(indoc! {r#"
            fn identity(x) { x }
            fn wrap(x) { [x] }
        "#});
        t.state_mut().set_stress_gc(true);
        // Chained calls test watermark save/restore with object values passing through.
        let result = t.eval(r#"wrap(identity(identity("hello")))"#);
        assert_eq!(t.state_mut().stringify(result), "[hello]");
    }

    #[test]
    fn test_stress_gc_deeply_nested_pattern_matching() {
        let mut t = interpreter(indoc! {r#"
            fn nested_match(list) {
                match list {
                    [[a, ...rest_inner], ...rest_outer] => [a, rest_inner, rest_outer],
                    _ => [],
                }
            }
        "#});
        t.state_mut().set_stress_gc(true);
        // Nested pattern creates multiple slices as rest values.
        let result = t.eval(r#"nested_match([["x", "y", "z"], "a", "b"])"#);
        assert_eq!(t.state_mut().stringify(result), "[x, &[y, z], &[a, b]]");
    }

    #[test]
    fn test_stress_gc_string_pattern_all_chars() {
        let mut t = interpreter(indoc! {r#"
            fn chars([a, b, c]) { [a, b, c] }
            fn chars(_) { [] }
        "#});
        t.state_mut().set_stress_gc(true);
        // Each character in the pattern creates a new_string allocation.
        let result = t.eval(r#"chars("abc")"#);
        assert_eq!(t.state_mut().stringify(result), "[a, b, c]");
    }

    #[test]
    fn test_stress_gc_partial_application() {
        let mut t = interpreter(indoc! {"
            fn add(a, b) { a + b }
        "});
        t.state_mut().set_stress_gc(true);
        // Partial application creates a native fn object capturing args.
        let result = t.eval("add(1, _)(2)");
        assert_matches!(result, TlangValue::U64(3));
    }

    #[test]
    fn test_stress_gc_partial_application_with_objects() {
        let mut t = interpreter(indoc! {r#"
            fn concat(a, b) { a + b }
        "#});
        t.state_mut().set_stress_gc(true);
        // Partial application captures a string object, then applies with another.
        let result = t.eval(r#"concat("hello ", _)("world")"#);
        assert_eq!(t.state_mut().stringify(result), "hello world");
    }

    #[test]
    fn test_stress_gc_foldl_with_string_concat() {
        let mut t = interpreter(indoc! {"
            fn foldl([], acc, _) { acc }
            fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
        "});
        t.state_mut().set_stress_gc(true);
        // foldl with string concatenation creates many intermediate strings.
        let result = t.eval(r#"foldl(["a", "b", "c", "d"], "", fn(acc, x) { acc + x })"#);
        assert_eq!(t.state_mut().stringify(result), "abcd");
    }

    #[test]
    fn test_stress_gc_enum_with_multiple_object_fields() {
        let mut t = interpreter(indoc! {"
            enum Entry {
                Pair(Int, Int),
            }
            fn first(e) {
                match e {
                    Entry::Pair(a, _) => a,
                }
            }
            fn second(e) {
                match e {
                    Entry::Pair(_, b) => b,
                }
            }
        "});
        t.state_mut().set_stress_gc(true);
        // Enum with two string fields, then destructure to access them.
        let result = t.eval(r#"first(Entry::Pair("key", "value"))"#);
        assert_eq!(t.state_mut().stringify(result), "key");
        let result = t.eval(r#"second(Entry::Pair("key", "value"))"#);
        assert_eq!(t.state_mut().stringify(result), "value");
    }

    // --- GC collection tests: verify unreachable objects are swept ---

    #[test]
    fn test_gc_collects_temporaries_after_string_concat() {
        // Wrap concatenation in a function call so the eval_call watermark
        // trims temp_roots when the call returns, making intermediates collectible.
        let mut t = interpreter(indoc! {r#"
            fn concat_three() { "hello" + " " + "world" }
        "#});
        let result = t.eval("concat_three()");
        assert_eq!(t.state_mut().stringify(result), "hello world");
        // After the call returns, temp_roots have been trimmed.
        // An explicit GC should collect the intermediate strings.
        let before = t.memory_stats().objects_deallocated;
        t.state_mut().collect_garbage();
        let after = t.memory_stats().objects_deallocated;
        assert!(
            after > before,
            "expected intermediate strings to be collected (before={before}, after={after})"
        );
    }

    #[test]
    fn test_gc_collects_temporaries_from_function_calls() {
        let mut t = interpreter(indoc! {r#"
            fn make_and_discard() {
                let a = "temporary1";
                let b = "temporary2";
                let c = "temporary3";
                42
            }
        "#});
        t.state_mut().set_stress_gc(true);
        let before_objects = t.object_count();
        let result = t.eval("make_and_discard()");
        assert_matches!(result, TlangValue::U64(42));
        // After the call returns, the 3 temporary strings should be collectible.
        t.state_mut().collect_garbage();
        let after_objects = t.object_count();
        assert!(
            after_objects <= before_objects,
            "expected temporary strings to be collected (before={before_objects}, after={after_objects})"
        );
    }

    #[test]
    fn test_gc_collects_intermediate_lists() {
        let mut t = interpreter(indoc! {"
            fn sum_list(list) {
                match list {
                    [] => 0,
                    [x, ...rest] => x + sum_list(rest),
                }
            }
        "});
        // sum_list uses non-tail recursion, so each recursive call goes through
        // eval_call with its own watermark. Intermediate slices from ...rest become
        // collectible as each call returns.
        let result = t.eval("sum_list([1, 2, 3, 4, 5])");
        assert_matches!(result, TlangValue::U64(15));
        // After the call, the original list and all intermediate slices should be collectible.
        t.state_mut().collect_garbage();
        let stats = t.memory_stats();
        assert!(
            stats.objects_deallocated > 0,
            "expected intermediate slices to be collected"
        );
    }

    #[test]
    fn test_gc_collects_abandoned_closures() {
        let mut t = interpreter(indoc! {"
            fn create_and_discard() {
                let f = fn(x) { x + 1 };
                let g = fn(x) { x * 2 };
                f(10)
            }
        "});
        t.state_mut().set_stress_gc(true);
        let result = t.eval("create_and_discard()");
        assert_matches!(result, TlangValue::U64(11));
        // After the call, both closures f and g should be unreachable.
        t.state_mut().collect_garbage();
        let stats = t.memory_stats();
        assert!(
            stats.objects_deallocated > 0,
            "expected abandoned closures to be collected"
        );
    }

    #[test]
    fn test_gc_collects_unreachable_structs() {
        let mut t = interpreter(indoc! {"
            struct Point {
                x: Int,
                y: Int,
            }
            fn make_point_x() {
                let p = Point { x: 1, y: 2 };
                p.x
            }
        "});
        let before = t.object_count();
        let result = t.eval("make_point_x()");
        assert_matches!(result, TlangValue::U64(1));
        // The Point struct is no longer reachable after extracting .x.
        t.state_mut().collect_garbage();
        let after = t.object_count();
        assert!(
            after <= before,
            "expected unreachable struct to be collected (before={before}, after={after})"
        );
    }

    #[test]
    fn test_gc_collects_unreachable_enum_variants() {
        let mut t = interpreter(indoc! {"
            enum Wrapper {
                Val(Int),
            }
            fn unwrap_val(w) {
                match w {
                    Wrapper::Val(v) => v,
                }
            }
        "});
        let before = t.object_count();
        let result = t.eval("unwrap_val(Wrapper::Val(99))");
        assert_matches!(result, TlangValue::U64(99));
        // The Wrapper::Val enum object is unreachable after unwrapping.
        t.state_mut().collect_garbage();
        let after = t.object_count();
        assert!(
            after <= before,
            "expected unreachable enum to be collected (before={before}, after={after})"
        );
    }

    #[test]
    fn test_gc_stress_many_allocations_bounded_heap() {
        // Use non-tail recursion so each call gets its own watermark.
        // When each call returns, its temp_roots are trimmed and temporaries
        // become collectible on the next stress-GC triggered allocation.
        let mut t = interpreter(indoc! {r#"
            fn loop_alloc(0) { "done" }
            fn loop_alloc(n) {
                let s = "temp" + "str";
                loop_alloc(n - 1)
            }
        "#});
        t.state_mut().set_stress_gc(true);
        let result = t.eval("loop_alloc(50)");
        assert_eq!(t.state_mut().stringify(result), "done");
        // After the top-level call returns, do a final collection.
        t.state_mut().collect_garbage();
        let stats = t.memory_stats();
        // Verify the GC actually ran and collected objects.
        assert!(
            stats.gc_collections > 10,
            "expected many GC cycles with stress_gc, got {}",
            stats.gc_collections
        );
        assert!(
            stats.objects_deallocated > 10,
            "expected many temporaries collected, got {}",
            stats.objects_deallocated
        );
        // Heap should be much smaller than total allocations.
        assert!(
            t.object_count() < stats.objects_allocated / 2,
            "heap should be bounded: {} live vs {} allocated",
            t.object_count(),
            stats.objects_allocated
        );
    }

    #[test]
    fn test_gc_recursive_string_processing_collects() {
        let mut t = interpreter(indoc! {r#"
            fn reverse(str) { reverse(str, "") }
            fn reverse("", acc) { acc }
            fn reverse([x, ...xs], acc) { rec reverse(xs, x + acc) }
        "#});
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"reverse("abcdefgh")"#);
        assert_eq!(t.state_mut().stringify(result), "hgfedcba");
        // After the call returns, temp_roots are trimmed. Explicit GC collects the rest.
        t.state_mut().collect_garbage();
        let stats = t.memory_stats();
        // Each step creates char strings, rest strings, and concat results.
        // After the call, only the final result string is live.
        assert!(
            stats.objects_deallocated > 0,
            "expected intermediates to be collected, got 0 deallocated out of {} allocated",
            stats.objects_allocated
        );
    }

    #[test]
    fn test_gc_map_creates_and_collects_intermediates() {
        let mut t = interpreter(indoc! {"
            fn map([], _) { [] }
            fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
            fn identity(x) { x }
        "});
        t.state_mut().set_stress_gc(true);
        let result = t.eval("map([1, 2, 3, 4, 5], identity)");
        assert_eq!(t.state_mut().stringify(result), "[1, 2, 3, 4, 5]");
        let stats = t.memory_stats();
        // map creates intermediate slices (from ...xs rest pattern) at each recursive step.
        // These slices become unreachable as recursion unwinds.
        assert!(
            stats.objects_deallocated > 0,
            "expected intermediate slices from map to be collected"
        );
    }

    #[test]
    fn test_stress_gc_regex_literal() {
        let mut t = interpreter("");
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"re"hello".test("hello world")"#);
        assert_eq!(result, TlangValue::Bool(true));
    }

    #[test]
    fn test_stress_gc_regex_exec() {
        let mut t = interpreter("");
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"re"\d+".exec("abc 42 def")"#);
        assert_eq!(t.state_mut().stringify(result), r#"Option::Some(0: 42)"#);
    }

    #[test]
    fn test_stress_gc_regex_replace() {
        let mut t = interpreter("");
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"re"world".replace_all("hello world", "tlang")"#);
        assert_eq!(t.state_mut().stringify(result), "hello tlang");
    }

    #[test]
    fn test_stress_gc_regex_in_loop() {
        let mut t = interpreter(indoc! {"
            fn count_matches(items) {
                let count = 0;
                for item in items {
                    if re\"a\".test(item) {
                        count = count + 1;
                    }
                }
                count
            }
        "});
        t.state_mut().set_stress_gc(true);
        let result =
            t.eval(r#"count_matches(["apple", "banana", "cherry", "avocado", "blueberry"])"#);
        assert_eq!(result, TlangValue::U64(3));
    }

    #[test]
    fn test_stress_gc_tagged_string_with_interpolation() {
        let mut t = interpreter(r#"let x = "world";"#);
        t.state_mut().set_stress_gc(true);
        let result = t.eval(r#"re"he{x}".test("helloworld")"#);
        assert_eq!(result, TlangValue::Bool(false));
    }

    #[test]
    fn test_native_map_list() {
        let mut t = interpreter("");
        let result = t.eval("map([1, 2, 3], fn(x) { x * 2 })");
        assert_eq!(t.state_mut().stringify(result), "[2, 4, 6]");
    }

    #[test]
    fn test_native_map_list_empty() {
        let mut t = interpreter("");
        let result = t.eval("map([], fn(x) { x * 2 })");
        assert_eq!(t.state_mut().stringify(result), "[]");
    }

    #[test]
    fn test_native_map_option_some() {
        let mut t = interpreter("");
        let result = t.eval("Option::Some(5).map(fn(x) { x * 2 })");
        assert_eq!(t.state_mut().stringify(result), "Option::Some(0: 10)");
    }

    #[test]
    fn test_native_map_option_none() {
        let mut t = interpreter("");
        let result = t.eval("Option::None.map(fn(x) { x * 2 })");
        assert_eq!(t.state_mut().stringify(result), "Option::None");
    }

    #[test]
    fn test_native_map_option_toplevel() {
        let mut t = interpreter("");
        let result = t.eval("map(Option::Some(3), fn(x) { x + 1 })");
        assert_eq!(t.state_mut().stringify(result), "Option::Some(0: 4)");

        let result = t.eval("map(Option::None, fn(x) { x + 1 })");
        assert_eq!(t.state_mut().stringify(result), "Option::None");
    }

    #[test]
    fn test_native_map_result_ok() {
        let mut t = interpreter("");
        let result = t.eval("Result::Ok(10).map(fn(x) { x + 5 })");
        assert_eq!(t.state_mut().stringify(result), "Result::Ok(0: 15)");
    }

    #[test]
    fn test_native_map_result_err() {
        let mut t = interpreter("");
        let result = t.eval("Result::Err(42).map(fn(x) { x + 5 })");
        assert_eq!(t.state_mut().stringify(result), "Result::Err(0: 42)");
    }

    #[test]
    fn test_native_map_result_toplevel() {
        let mut t = interpreter("");
        let result = t.eval("map(Result::Ok(7), fn(x) { x * 3 })");
        assert_eq!(t.state_mut().stringify(result), "Result::Ok(0: 21)");

        let result = t.eval("map(Result::Err(99), fn(x) { x * 3 })");
        assert_eq!(t.state_mut().stringify(result), "Result::Err(0: 99)");
    }

    #[test]
    fn test_native_map_slice() {
        let mut t = interpreter("");
        // Use the slice method to get a slice, then map over it
        let result = t.eval("[1, 2, 3].slice(1) |> map(fn(x) { x * 10 })");
        assert_eq!(t.state_mut().stringify(result), "[20, 30]");
    }

    #[test]
    fn test_native_map_string() {
        let mut t = interpreter("");
        // map over string maps over each character
        let result = t.eval(r#"map("abc", fn(c) { c + c })"#);
        assert_eq!(t.state_mut().stringify(result), "aabbcc");
    }

    // -------------------------------------------------------------------------
    // Pipeline debug tests for multi-arity dispatch (add/1 + add/2 + DynFnDecl)
    // -------------------------------------------------------------------------
    const MULTI_DISPATCH_SRC: &str = indoc! {"
        fn add(a, b) { a + b }
        fn add(a) { add(a, 1) }
        add(5) |> log();
        let add2 = add;
        add2(10) |> log();
        add2(3, 4) |> log();
    "};

    /// Parse + semantic-analyse, then lower WITHOUT any optimizer.
    fn pipeline_lower(
        src: &str,
    ) -> (
        hir::Module,
        hir::LowerResultMeta,
        tlang_semantics::SemanticAnalyzer,
    ) {
        let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols_with_slots(&tlang_vm::VM::builtin_symbols());

        let mut parser = tlang_parser::Parser::from_source(src);
        let (mut ast, parse_meta) = parser.parse().unwrap();
        semantic_analyzer
            .analyze_as_root_module(&mut ast)
            .expect("semantic analysis failed");

        let mut lowering_context = tlang_ast_lowering::LoweringContext::new(
            semantic_analyzer.symbol_id_allocator(),
            semantic_analyzer.root_symbol_table(),
            semantic_analyzer.symbol_tables().clone(),
        );
        let (module, meta) = tlang_ast_lowering::lower(
            &mut lowering_context,
            &ast,
            &parse_meta.constant_pool_node_ids,
        )
        .expect("lowering failed");

        (module, meta, semantic_analyzer)
    }

    /// Verify semantic analysis produces correct symbol table entries for
    /// multi-arity `add`: two Function variants and one Variable (DynFnDecl).
    #[test]
    fn test_multi_dispatch_semantic_analysis() {
        let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols_with_slots(&tlang_vm::VM::builtin_symbols());

        let mut parser = tlang_parser::Parser::from_source(MULTI_DISPATCH_SRC);
        let (mut ast, _) = parser.parse().unwrap();
        semantic_analyzer
            .analyze_as_root_module(&mut ast)
            .expect("semantic analysis failed");

        let root = semantic_analyzer.root_symbol_table();
        let root = root.borrow();
        let add_syms: Vec<_> = root
            .get_all_local_symbols()
            .iter()
            .filter(|s| *s.name == *"add")
            .collect();

        eprintln!("add symbols after semantic analysis: {add_syms:#?}");

        // Expect exactly two Function variants (arity 1 and 2) at this point.
        // The Variable (DynFnDecl) entry is inserted during HIR lowering, not here.
        let fn_variants: Vec<_> = add_syms
            .iter()
            .filter(|s| matches!(s.kind, tlang_defs::DefKind::Function(_)))
            .collect();
        assert_eq!(
            fn_variants.len(),
            2,
            "Expected 2 Function entries for `add` after semantic analysis, got: {add_syms:#?}"
        );

        let arities: std::collections::HashSet<u16> =
            fn_variants.iter().filter_map(|s| s.kind.arity()).collect();
        assert!(
            arities.contains(&1),
            "Missing arity-1 variant in symbol table: {add_syms:#?}"
        );
        assert!(
            arities.contains(&2),
            "Missing arity-2 variant in symbol table: {add_syms:#?}"
        );
    }

    /// Verify HIR lowering produces FnDecl(add/2), FnDecl(add/1), DynFnDecl(add)
    /// with matching variant hir_ids.
    #[test]
    fn test_multi_dispatch_lowering() {
        let (module, _meta, _sem) = pipeline_lower(MULTI_DISPATCH_SRC);

        let fn_decls: Vec<_> = module
            .block
            .stmts
            .iter()
            .filter_map(|s| {
                if let hir::StmtKind::FunctionDeclaration(f) = &s.kind {
                    Some((f.hir_id, f.name()))
                } else {
                    None
                }
            })
            .collect();
        let dyn_decls: Vec<_> = module
            .block
            .stmts
            .iter()
            .filter_map(|s| {
                if let hir::StmtKind::DynFunctionDeclaration(d) = &s.kind {
                    Some(d.as_ref())
                } else {
                    None
                }
            })
            .collect();

        eprintln!("FnDecls after lowering: {fn_decls:#?}");
        eprintln!("DynFnDecls after lowering: {dyn_decls:#?}");

        assert_eq!(fn_decls.len(), 2, "Expected 2 FnDecls (add/2 and add/1)");
        assert_eq!(dyn_decls.len(), 1, "Expected 1 DynFnDecl");

        let dyn_decl = dyn_decls[0];
        let arities: Vec<usize> = dyn_decl.variants.iter().map(|(a, _)| *a).collect();
        assert_eq!(
            arities,
            vec![2, 1],
            "DynFnDecl should list variants in source order (arity 2 first, then 1)"
        );

        let fn_hir_ids: Vec<_> = fn_decls.iter().map(|(id, _)| *id).collect();
        for (arity, variant_id) in &dyn_decl.variants {
            assert!(
                fn_hir_ids.contains(variant_id),
                "DynFnDecl arity-{arity} variant hir_id {variant_id:?} not in FnDecl ids {fn_hir_ids:?}"
            );
        }
    }

    /// Run the full default optimizer pipeline and verify it does not panic,
    /// then confirm the interpreter produces 6, 11, 7.
    #[test]
    fn test_multi_dispatch_full_pipeline() {
        let (mut module, meta, _sem) = pipeline_lower(MULTI_DISPATCH_SRC);

        let mut optimizer = HirOptimizer::default();
        let constant_pool_ids = meta.constant_pool_ids.clone();
        let mut ctx = meta.into();
        optimizer
            .optimize_hir(&mut module, &mut ctx)
            .expect("optimizer failed");

        // Log what the module looks like after full optimization for diagnostics
        let stmt_kinds: Vec<_> = module
            .block
            .stmts
            .iter()
            .map(|s| match &s.kind {
                hir::StmtKind::FunctionDeclaration(f) => {
                    format!("FnDecl({}#{:?})", f.name(), f.hir_id)
                }
                hir::StmtKind::DynFunctionDeclaration(d) => format!("DynFnDecl({:?})", d.hir_id),
                hir::StmtKind::Let(..) => "Let".to_string(),
                hir::StmtKind::Expr(_) => "Expr".to_string(),
                other => format!("{other:?}"),
            })
            .collect();
        eprintln!("Top-level stmts after full optimization: {stmt_kinds:#?}");

        let vm = tlang_vm::VM::new();
        let mut state = vm.into_state();
        state.register_constant_pool_ids(constant_pool_ids);

        let logged = Rc::new(RefCell::new(vec![]));
        let logged_clone = logged.clone();
        let log_fn = state.new_native_fn("log", move |state, args| {
            let msg = args
                .iter()
                .map(|v| state.stringify(*v))
                .collect::<Vec<_>>()
                .join(", ");
            logged_clone.borrow_mut().push(msg);
            NativeFnReturn::Return(TlangValue::Nil)
        });
        state.set_global("log".to_string(), log_fn);

        Interpreter.eval(&mut state, &module);

        let output = logged.borrow().clone();
        assert_eq!(
            output,
            vec!["6".to_string(), "11".to_string(), "7".to_string()],
            "Expected log output [6, 11, 7] but got: {output:#?}"
        );
    }

    /// Regression test: optimizer must not disturb slot-ordering of multi-dispatch variants.
    ///
    /// The HirOptimizer runs multiple passes until convergence. This test verifies that after
    /// full optimization the FnDecls appear in the same source order as the symbol-table records
    /// them, so interpreter slot assignments stay consistent.
    #[test]
    fn test_multi_dispatch_optimizer_bisect() {
        let (mut module, meta, _sem) = pipeline_lower(MULTI_DISPATCH_SRC);
        let constant_pool_ids = meta.constant_pool_ids.clone();
        let mut ctx = meta.into();

        let mut optimizer = HirOptimizer::default();
        optimizer
            .optimize_hir(&mut module, &mut ctx)
            .expect("optimizer failed");

        // FnDecls must be emitted in source order (add/2 first, add/1 second) so that
        // interpreter slot assignments match the symbol-table order established during
        // semantic analysis.
        let fn_decl_names: Vec<_> = module
            .block
            .stmts
            .iter()
            .filter_map(|s| {
                if let hir::StmtKind::FunctionDeclaration(f) = &s.kind {
                    Some(f.name())
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(
            fn_decl_names,
            vec!["add/2", "add/1"],
            "FnDecls must be in source order; got: {fn_decl_names:?}"
        );

        let vm = tlang_vm::VM::new();
        let mut state = vm.into_state();
        state.register_constant_pool_ids(constant_pool_ids);

        let logged = Rc::new(RefCell::new(vec![]));
        let logged_clone = logged.clone();
        let log_fn = state.new_native_fn("log", move |state, args| {
            let msg = args
                .iter()
                .map(|v| state.stringify(*v))
                .collect::<Vec<_>>()
                .join(", ");
            logged_clone.borrow_mut().push(msg);
            NativeFnReturn::Return(TlangValue::Nil)
        });
        state.set_global("log".to_string(), log_fn);

        Interpreter.eval(&mut state, &module);

        let output = logged.borrow().clone();
        assert_eq!(
            output,
            vec!["6", "11", "7"],
            "Expected [6, 11, 7]: {output:?}"
        );
    }
}
