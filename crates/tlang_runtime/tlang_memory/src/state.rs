use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use slab::Slab;
use tlang_hir::hir::{self, HirId};

use crate::resolver::Resolver;
use crate::scope::{Scope, ScopeStack};
use crate::shape::{ShapeKey, TlangStructMethod, TlangStructShape};
use crate::value::{
    NativeFnReturn, TlangClosure, TlangNativeFn, TlangObjectId, TlangObjectKind, TlangSlice,
    TlangStruct, TlangValue,
};

pub enum CallStackKind {
    Root,
    Function(Rc<hir::FunctionDeclaration>),
    NativeFn(String),
}

pub struct CallStackEntry {
    pub kind: CallStackKind,
    pub tail_call: Option<TailCall>,
    pub current_span: tlang_ast::span::Span,
}

impl CallStackEntry {
    pub fn new_call(fn_decl: Rc<hir::FunctionDeclaration>) -> Self {
        Self {
            kind: CallStackKind::Function(fn_decl.clone()),
            tail_call: None,
            current_span: fn_decl.span,
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

pub struct BuiltinShapes {
    pub list: ShapeKey,
    pub shapes: Slab<TlangStructShape>,
}

impl Default for BuiltinShapes {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinShapes {
    pub fn new() -> Self {
        let mut shapes = Slab::with_capacity(1);

        let list = ShapeKey::new_native(shapes.insert(TlangStructShape::new(
            "List".to_string(),
            vec![],
            HashMap::new(),
        )));

        Self { list, shapes }
    }

    pub fn get_list_shape(&self) -> &TlangStructShape {
        self.shapes.get(self.list.get_native_index()).unwrap()
    }

    pub fn get_list_shape_mut(&mut self) -> &mut TlangStructShape {
        self.shapes.get_mut(self.list.get_native_index()).unwrap()
    }
}

#[derive(Debug)]
pub struct TailCall {
    pub callee: TlangValue,
    pub args: Vec<TlangValue>,
}

pub struct InterpreterState {
    pub scope_stack: ScopeStack,
    closures: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    objects: Slab<TlangObjectKind>,
    shapes: HashMap<ShapeKey, TlangStructShape>,
    fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
    call_stack: Vec<CallStackEntry>,
    globals: HashMap<String, TlangValue>,
    pub builtin_shapes: BuiltinShapes,
    native_fns: HashMap<TlangObjectId, TlangNativeFn>,
}

impl Resolver for InterpreterState {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        let value = self
            .scope_stack
            .resolve_path(path)
            .or_else(|| self.globals.get(&path.join("::")).copied());

        debug!(
            "Resolved path: {:?} ({:?}), got: {:?}",
            path.join("::"),
            path.res,
            value.map(|v| self.stringify(v))
        );

        value
    }
}

impl InterpreterState {
    pub fn new() -> Self {
        let mut call_stack = Vec::with_capacity(1000);

        call_stack.push(CallStackEntry {
            kind: CallStackKind::Root,
            tail_call: None,
            current_span: tlang_ast::span::Span::default(),
        });

        Self {
            scope_stack: ScopeStack::default(),
            closures: HashMap::with_capacity(100),
            objects: Slab::with_capacity(1000),
            struct_decls: HashMap::with_capacity(100),
            fn_decls: HashMap::with_capacity(1000),
            shapes: HashMap::with_capacity(100),
            call_stack,
            globals: HashMap::with_capacity(100),
            builtin_shapes: BuiltinShapes::default(),
            native_fns: HashMap::with_capacity(100),
        }
    }

    pub fn get_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.fn_decls.get(&id).cloned()
    }

    pub fn set_fn_decl(&mut self, id: hir::HirId, decl: Rc<hir::FunctionDeclaration>) {
        self.fn_decls.insert(id, decl);
    }

    pub fn get_struct_decl(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        let path_name = path.join("::");

        self.struct_decls.get(&path_name).cloned()
    }

    pub fn set_struct_decl(&mut self, path_name: String, decl: Rc<hir::StructDeclaration>) {
        self.struct_decls.insert(path_name, decl);
    }

    pub fn panic(&self, message: String) -> ! {
        let mut call_stack = String::new();

        for entry in self.call_stack.iter().rev() {
            match &entry.kind {
                CallStackKind::Function(decl) => {
                    call_stack.push_str(&format!(
                        "  at function {}:{}\n",
                        decl.name(),
                        entry.current_span.start
                    ));
                }
                CallStackKind::NativeFn(name) => {
                    call_stack.push_str(&format!("  at native function {}:0:0\n", name));
                }
                CallStackKind::Root => {
                    call_stack.push_str(&format!("  at root {}\n", entry.current_span.start));
                }
            }
        }

        panic!("{}\n{}", message, call_stack)
    }

    pub fn set_current_span(&mut self, span: tlang_ast::span::Span) {
        self.call_stack.last_mut().unwrap().current_span = span;
    }

    pub fn push_call_stack(&mut self, entry: CallStackEntry) {
        self.call_stack.push(entry);
    }

    pub fn pop_call_stack(&mut self) -> CallStackEntry {
        self.call_stack.pop().unwrap()
    }

    pub fn current_call_frame(&mut self) -> &CallStackEntry {
        self.call_stack.last().unwrap()
    }

    pub fn current_call_frame_mut(&mut self) -> &mut CallStackEntry {
        self.call_stack.last_mut().unwrap()
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

    pub fn current_scope(&self) -> Rc<RefCell<Scope>> {
        self.scope_stack.current_scope()
    }

    pub fn set_global(&mut self, name: String, value: TlangValue) {
        self.globals.insert(name, value);
    }

    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        TlangValue::new_object(self.objects.insert(kind))
    }

    pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
        self.closures
            .entry(decl.hir_id)
            .or_insert_with(|| decl.clone().into());

        self.new_object(TlangObjectKind::Closure(TlangClosure {
            id: decl.hir_id,
            scope_stack: self.scope_stack.clone(),
        }))
    }

    pub fn new_native_fn<F>(&mut self, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.new_object(TlangObjectKind::NativeFn);

        self.native_fns
            .insert(fn_object.get_object_id().unwrap(), Box::new(f));

        fn_object
    }

    pub fn call_native_fn(
        &mut self,
        fn_id: TlangObjectId,
        args: &[TlangValue],
    ) -> Option<NativeFnReturn> {
        let native_fn_ptr = self.native_fns.get_mut(&fn_id)? as *mut TlangNativeFn;

        // 🙈
        unsafe {
            let native_fn = &mut *native_fn_ptr;
            Some(native_fn(self, args))
        }
    }

    pub fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.closures.get(&id).cloned()
    }

    #[inline(always)]
    pub fn get_object_by_id(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.objects.get(id)
    }

    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        value
            .get_object_id()
            .and_then(|id| self.get_object_by_id(id))
    }

    pub fn get_struct(&self, value: TlangValue) -> Option<&TlangStruct> {
        self.get_object(value).and_then(|obj| obj.get_struct())
    }

    pub fn get_slice(&self, value: TlangValue) -> Option<TlangSlice> {
        self.get_object(value).and_then(|obj| obj.get_slice())
    }

    pub fn get_slice_value(&self, slice: TlangSlice, index: usize) -> TlangValue {
        let list = self.get_struct(slice.of()).unwrap();
        *list.field_values.get(slice.start() + index).unwrap()
    }

    pub fn get_slice_values(&self, slice: TlangSlice) -> &[TlangValue] {
        let list = self.get_struct(slice.of()).unwrap();
        &list.field_values[slice.range()]
    }

    pub fn new_list(&mut self, values: Vec<TlangValue>) -> TlangValue {
        self.new_object(TlangObjectKind::Struct(TlangStruct {
            shape: self.builtin_shapes.list,
            field_values: values,
        }))
    }

    pub fn new_string(&mut self, value: String) -> TlangValue {
        self.new_object(TlangObjectKind::String(value))
    }

    pub fn new_slice(&mut self, of: TlangValue, start: usize, len: usize) -> TlangValue {
        self.new_object(TlangObjectKind::Slice(TlangSlice::new(of, start, len)))
    }

    pub fn define_struct_shape(
        &mut self,
        shape_key: ShapeKey,
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> ShapeKey {
        let shape = TlangStructShape::new(name, fields, methods);
        self.shapes.insert(shape_key, shape);
        shape_key
    }

    pub fn set_shape(&mut self, key: ShapeKey, shape: TlangStructShape) {
        self.shapes.insert(key, shape);
    }

    pub fn get_shape(&self, key: ShapeKey) -> Option<&TlangStructShape> {
        match key {
            ShapeKey::Native(idx) => self.builtin_shapes.shapes.get(idx),
            key => self.shapes.get(&key),
        }
    }

    pub fn get_field_index(&self, shape: ShapeKey, field: &str) -> Option<usize> {
        self.shapes
            .get(&shape)
            .and_then(|shape| shape.get_field_index(field))
    }

    pub fn set_struct_method(
        &mut self,
        shape: ShapeKey,
        method_name: &str,
        method: TlangStructMethod,
    ) {
        self.shapes
            .get_mut(&shape)
            .and_then(|shape| shape.method_map.insert(method_name.to_string(), method));
    }

    fn stringify_struct_as_list(&self, s: &TlangStruct) -> String {
        let values = s
            .field_values
            .iter()
            .map(|v| self.stringify(*v))
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", values)
    }

    pub fn stringify(&self, value: TlangValue) -> String {
        match value {
            TlangValue::Object(id) => match self.get_object_by_id(id) {
                Some(TlangObjectKind::String(s)) => s.clone(),
                Some(TlangObjectKind::Struct(s)) => {
                    if s.shape == self.builtin_shapes.list {
                        return self.stringify_struct_as_list(s);
                    }

                    let shape = self.get_shape(s.shape).unwrap();

                    if shape.has_fields() && shape.has_consecutive_integer_fields() {
                        return format!("{} {}", shape.name, self.stringify_struct_as_list(s));
                    }

                    let mut result = String::new();

                    result.push_str(&shape.name);
                    result.push_str(" { ");

                    if !shape.has_fields() {
                        result.push('}');
                        return result;
                    }

                    let mut fields = shape.get_fields();
                    fields.sort();
                    result.push_str(
                        &fields
                            .into_iter()
                            .map(|(field, idx)| {
                                format!("{}: {}", field, self.stringify(s.field_values[idx]))
                            })
                            .collect::<Vec<String>>()
                            .join(", "),
                    );

                    result.push_str(" }");
                    result
                }
                Some(TlangObjectKind::Slice(s)) => {
                    let values = self
                        .get_slice_values(*s)
                        .iter()
                        .map(|v| self.stringify(*v))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("&[{}]", values)
                }
                Some(TlangObjectKind::Closure(s)) => {
                    let fn_decl = self.closures.get(&s.id).unwrap();

                    format!("fn {}({:?})", fn_decl.name(), s.id)
                }
                _ => format!("{:?}", value),
            },
            _ => value.to_string(),
        }
    }
}

impl Default for InterpreterState {
    fn default() -> Self {
        Self::new()
    }
}
