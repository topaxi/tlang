use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_hir::hir::{self, HirId};

use crate::resolver::Resolver;
use crate::scope::Scope;
use crate::value::{
    ShapeKey, TlangObjectId, TlangObjectKind, TlangStruct, TlangStructMethod, TlangStructShape,
    TlangValue,
};

pub enum CallStackKind {
    Root,
    Function(Rc<hir::FunctionDeclaration>),
    NativeFn(String),
}

pub struct CallStackEntry {
    pub kind: CallStackKind,
    pub current_span: tlang_ast::span::Span,
}

pub struct InterpreterState {
    pub(crate) root_scope: Rc<RefCell<Scope>>,
    pub(crate) current_scope: Rc<RefCell<Scope>>,
    pub(crate) closures: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    pub(crate) objects: HashMap<TlangObjectId, TlangObjectKind>,
    pub(crate) shapes: HashMap<ShapeKey, TlangStructShape>,
    pub(crate) fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    pub(crate) struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
    pub(crate) call_stack: Vec<CallStackEntry>,
    pub list_shape: ShapeKey,
}

impl Resolver for InterpreterState {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.current_scope.borrow().resolve_path(path)
    }
}

impl InterpreterState {
    pub(crate) fn new(list_shape: ShapeKey) -> Self {
        let root_scope = Rc::new(RefCell::new(Scope::default()));
        let current_scope = root_scope.clone();
        let mut call_stack = Vec::with_capacity(1000);

        call_stack.push(CallStackEntry {
            kind: CallStackKind::Root,
            current_span: tlang_ast::span::Span::default(),
        });

        Self {
            root_scope,
            current_scope,
            closures: HashMap::with_capacity(100),
            objects: HashMap::with_capacity(1000),
            struct_decls: HashMap::with_capacity(100),
            fn_decls: HashMap::with_capacity(1000),
            shapes: HashMap::with_capacity(100),
            call_stack,
            list_shape,
        }
    }

    pub(crate) fn get_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        if let Some(decl) = self.fn_decls.get(&id) {
            return Some(decl.clone());
        }

        None
    }

    pub(crate) fn get_struct_decl(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        let path_name = path.join("::");

        if let Some(decl) = self.struct_decls.get(&path_name) {
            return Some(decl.clone());
        }

        None
    }

    pub(crate) fn panic(&self, message: String) -> ! {
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

    pub(crate) fn set_current_span(&mut self, span: tlang_ast::span::Span) {
        self.call_stack.last_mut().unwrap().current_span = span;
    }

    pub(crate) fn enter_scope(&mut self) {
        let child_scope = Scope::new_child(self.current_scope.clone());
        self.current_scope = Rc::new(RefCell::new(child_scope));
    }

    pub(crate) fn exit_scope(&mut self) {
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

    #[inline(always)]
    pub(crate) fn get_object_by_id(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.objects.get(&id)
    }

    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        value
            .get_object_id()
            .and_then(|id| self.get_object_by_id(id))
    }

    pub fn new_list(&mut self, values: Vec<TlangValue>) -> TlangValue {
        self.new_object(TlangObjectKind::Struct(TlangStruct {
            shape: self.list_shape,
            field_values: values,
        }))
    }

    pub fn new_string(&mut self, value: String) -> TlangValue {
        self.new_object(TlangObjectKind::String(value))
    }

    pub fn define_native_struct(
        &mut self,
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> ShapeKey {
        let shape_key = ShapeKey::new_native();
        self.define_struct_shape(shape_key, name, fields, methods)
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

    pub fn get_shape(&self, key: ShapeKey) -> Option<&TlangStructShape> {
        self.shapes.get(&key)
    }

    pub fn get_field_index(&self, shape: ShapeKey, field: &str) -> Option<usize> {
        self.shapes
            .get(&shape)
            .and_then(|shape| shape.field_map.get(field).copied())
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

    pub fn stringify(&self, value: &TlangValue) -> String {
        match value {
            TlangValue::Object(id) => match self.get_object_by_id(*id) {
                Some(TlangObjectKind::String(s)) => s.clone(),
                Some(TlangObjectKind::Struct(s)) => {
                    if s.shape == self.list_shape {
                        let values = s
                            .field_values
                            .iter()
                            .map(|v| self.stringify(v))
                            .collect::<Vec<String>>()
                            .join(", ");
                        return format!("[{}]", values);
                    }

                    let shape = self.get_shape(s.shape).unwrap();
                    let mut result = String::new();

                    result.push_str(&shape.name);
                    result.push_str(" { ");

                    if shape.field_map.is_empty() {
                        result.push('}');
                        return result;
                    }

                    let mut fields = shape
                        .field_map
                        .clone()
                        .into_iter()
                        .collect::<Vec<(String, usize)>>();
                    fields.sort();
                    result.push_str(
                        &fields
                            .into_iter()
                            .map(|(field, idx)| {
                                let value = &s.field_values[idx];
                                format!("{}: {}", field, self.stringify(value))
                            })
                            .collect::<Vec<String>>()
                            .join(", "),
                    );

                    result.push_str(" }");
                    result
                }
                _ => format!("{:?}", value),
            },
            _ => value.to_string(),
        }
    }
}
