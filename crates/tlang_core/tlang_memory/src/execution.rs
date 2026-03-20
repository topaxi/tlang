use std::rc::Rc;

use smallvec::SmallVec;
use tlang_hir as hir;

use crate::scope::{Scope, ScopeStack};
use crate::value::TlangValue;

// ─── CallStack types ──────────────────────────────────────────────────────────

pub enum CallStackKind {
    Root,
    Function(Rc<hir::FunctionDeclaration>),
    NativeFn(String),
}

pub struct CallStackEntry {
    pub kind: CallStackKind,
    pub tail_call: Option<TailCall>,
    pub current_span: tlang_span::Span,
}

impl CallStackEntry {
    pub fn new_call(fn_decl: &Rc<hir::FunctionDeclaration>) -> Self {
        Self {
            kind: CallStackKind::Function(fn_decl.clone()),
            tail_call: None,
            current_span: fn_decl.span,
        }
    }

    pub fn new_native_call(name: &str) -> Self {
        Self {
            kind: CallStackKind::NativeFn(name.to_string()),
            tail_call: None,
            current_span: Default::default(),
        }
    }

    pub fn replace_fn_decl(&mut self, fn_decl: Rc<hir::FunctionDeclaration>) {
        self.kind = CallStackKind::Function(fn_decl);
    }

    pub fn set_tail_call(&mut self, tail_call: TailCall) {
        self.tail_call = Some(tail_call);
    }

    pub fn get_fn_decl(&self) -> Option<Rc<hir::FunctionDeclaration>> {
        match &self.kind {
            CallStackKind::Function(fn_decl) => Some(fn_decl.clone()),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct TailCall {
    pub callee: TlangValue,
    pub args: SmallVec<[TlangValue; 4]>,
}

// ─── ExecutionContext ─────────────────────────────────────────────────────────

/// Per-execution runtime state: the scope stack and call frames.
pub struct ExecutionContext {
    pub scope_stack: ScopeStack,
    pub(crate) call_stack: Vec<CallStackEntry>,
}

impl ExecutionContext {
    pub(crate) fn new() -> Self {
        let mut call_stack = Vec::with_capacity(1000);
        call_stack.push(CallStackEntry {
            kind: CallStackKind::Root,
            tail_call: None,
            current_span: tlang_span::Span::default(),
        });
        Self {
            scope_stack: ScopeStack::default(),
            call_stack,
        }
    }

    pub fn push_call_stack(&mut self, entry: CallStackEntry) {
        self.call_stack.push(entry);
    }

    /// # Panics
    pub fn pop_call_stack(&mut self) -> CallStackEntry {
        self.call_stack.pop().unwrap()
    }

    /// # Panics
    pub fn current_call_frame(&self) -> &CallStackEntry {
        self.call_stack.last().unwrap()
    }

    /// # Panics
    pub fn current_call_frame_mut(&mut self) -> &mut CallStackEntry {
        self.call_stack.last_mut().unwrap()
    }

    /// # Panics
    pub fn set_current_span(&mut self, span: tlang_span::Span) {
        self.call_stack.last_mut().unwrap().current_span = span;
    }

    pub fn enter_scope<T>(&mut self, meta: &T)
    where
        T: hir::HirScope,
    {
        self.scope_stack.push(meta);
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn current_scope(&self) -> &Scope {
        self.scope_stack.current_scope()
    }

    pub fn push_value(&mut self, value: TlangValue) {
        self.scope_stack.push_value(value);
    }

    /// Allocate a variable index for let bindings and set the value at that position.
    pub fn set_let_binding(&mut self, value: TlangValue) -> usize {
        let index = self.scope_stack.allocate_let_binding_index();
        self.scope_stack.set_local(index, value);
        index
    }

    /// Initialize variable index counter after function parameters are pushed.
    pub fn init_var_index_after_params(&mut self, param_count: usize) {
        self.scope_stack.init_var_index_after_params(param_count);
    }

    /// Check if we're currently in the global scope.
    pub fn is_global_scope(&self) -> bool {
        self.scope_stack.scopes.len() == 1
    }

    /// Check if the current scope has allocated slots for variables.
    pub fn current_scope_has_slots(&self) -> bool {
        self.scope_stack.current_scope_has_slots()
    }

    /// Returns an iterator over GC roots in the execution context (live scope values).
    pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        self.scope_stack.memory_iter()
    }
}
