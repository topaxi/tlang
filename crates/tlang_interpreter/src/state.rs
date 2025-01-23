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

pub struct InterpreterState {
    pub(crate) root_scope: Rc<RefCell<Scope>>,
    pub(crate) current_scope: Rc<RefCell<Scope>>,
    pub(crate) closures: HashMap<HirId, hir::FunctionDeclaration>,
    pub(crate) objects: HashMap<TlangObjectId, TlangObjectKind>,
    pub(crate) shapes: HashMap<ShapeKey, TlangStructShape>,
    pub(crate) list_shape: ShapeKey,
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
    pub(crate) fn new(list_shape: ShapeKey) -> Self {
        let root_scope = Rc::new(RefCell::new(Scope::default()));
        let current_scope = root_scope.clone();

        Self {
            root_scope,
            current_scope,
            closures: HashMap::new(),
            objects: HashMap::new(),
            shapes: HashMap::new(),
            list_shape,
        }
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

    pub fn get_object(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.objects.get(&id)
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
}
