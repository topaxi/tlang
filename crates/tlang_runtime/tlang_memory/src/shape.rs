use std::collections::HashMap;
use std::hash::DefaultHasher;

use tlang_hir::hir::HirId;

use crate::value::{TlangObjectId, TlangValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeKey {
    HirId(HirId),
    Native(usize),
    // ShapeKeyImpl::Dict is a hash value generated from each of the keys in the struct.
    Dict(u64),
}

impl ShapeKey {
    pub fn new_native(index: usize) -> Self {
        ShapeKey::Native(index)
    }

    pub fn get_native_index(self) -> usize {
        match self {
            ShapeKey::Native(index) => index,
            _ => panic!("Expected native shape key, got {:?}", self),
        }
    }

    pub fn new_hir_id(id: HirId) -> Self {
        ShapeKey::HirId(id)
    }

    pub fn from_dict_keys(keys: &[String]) -> Self {
        let mut hasher = DefaultHasher::new();

        for key in keys.iter() {
            std::hash::Hash::hash(&key, &mut hasher);
        }

        let hash = std::hash::Hasher::finish(&hasher);

        ShapeKey::Dict(hash)
    }
}

impl From<HirId> for ShapeKey {
    fn from(id: HirId) -> Self {
        ShapeKey::new_hir_id(id)
    }
}

pub enum TlangShape {
    Struct(TlangStructShape),
    Enum(TlangEnumShape),
}

impl TlangShape {
    pub fn new_struct_shape(
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> Self {
        TlangShape::Struct(TlangStructShape::new(name, fields, methods))
    }

    pub fn new_enum_shape(
        name: String,
        variants: Vec<TlangEnumVariant>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> Self {
        TlangShape::Enum(TlangEnumShape {
            name,
            variants,
            method_map: methods,
        })
    }

    pub fn get_struct_shape(&self) -> Option<&TlangStructShape> {
        match self {
            TlangShape::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_struct_shape_mut(&mut self) -> Option<&mut TlangStructShape> {
        match self {
            TlangShape::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_enum_shape(&self) -> Option<&TlangEnumShape> {
        match self {
            TlangShape::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn get_enum_shape_mut(&mut self) -> Option<&mut TlangEnumShape> {
        match self {
            TlangShape::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            TlangShape::Struct(s) => &s.name,
            TlangShape::Enum(e) => &e.name,
        }
    }

    pub fn get_method(&self, name: &str) -> Option<&TlangStructMethod> {
        match self {
            TlangShape::Struct(s) => s.get_method(name),
            TlangShape::Enum(e) => e.get_method(name),
        }
    }

    pub fn get_method_names(&self) -> Vec<String> {
        match self {
            TlangShape::Struct(s) => s.method_map.keys().cloned().collect(),
            TlangShape::Enum(e) => e.method_map.keys().cloned().collect(),
        }
    }
}

pub struct TlangStructShape {
    pub name: String,
    pub field_map: HashMap<String, usize>,
    pub method_map: HashMap<String, TlangStructMethod>,
}

impl TlangStructShape {
    pub fn new(
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> Self {
        let field_map = fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| (f, i))
            .collect();

        Self {
            name,
            field_map,
            method_map: methods,
        }
    }

    pub fn has_fields(&self) -> bool {
        !self.field_map.is_empty()
    }

    pub fn has_consecutive_integer_fields(&self) -> bool {
        let mut fields = self.field_map.keys().map(|k| k.parse::<usize>());

        if fields.any(|f| f.is_err()) {
            return false;
        }

        let mut fields = fields.map(|f| f.unwrap()).collect::<Vec<_>>();

        fields.sort();

        for (i, k) in fields.into_iter().enumerate() {
            if i == k {
                continue;
            }

            return false;
        }

        true
    }

    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.field_map.get(name).copied()
    }

    pub fn get_fields(&self) -> Vec<(String, usize)> {
        self.field_map
            .iter()
            .map(|(k, v)| (k.clone(), *v))
            .collect()
    }

    pub fn get_method(&self, name: &str) -> Option<&TlangStructMethod> {
        self.method_map.get(name)
    }

    pub fn set_fields(&mut self, fields: Vec<String>) {
        self.field_map = fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| (f, i))
            .collect();
    }

    pub fn set_methods(&mut self, methods: HashMap<String, TlangStructMethod>) {
        self.method_map = methods;
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TlangStructMethod {
    Native(TlangObjectId),
    HirId(HirId),
}

impl TlangStructMethod {
    pub fn from_value(value: TlangValue) -> Self {
        Self::from_object_id(value.get_object_id().unwrap())
    }

    pub fn from_object_id(id: TlangObjectId) -> Self {
        TlangStructMethod::Native(id)
    }
}

#[derive(Debug)]
pub struct TlangEnumVariant {
    pub name: String,
    /// Field names to positions, empty for positional variants.
    pub field_map: HashMap<String, usize>,
}

impl TlangEnumVariant {
    pub fn new(name: String, field_map: HashMap<String, usize>) -> Self {
        Self { name, field_map }
    }

    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.field_map.get(name).copied()
    }
}

#[derive(Debug)]
pub struct TlangEnumShape {
    pub name: String,
    pub variants: Vec<TlangEnumVariant>,
    pub method_map: HashMap<String, TlangStructMethod>,
}

impl TlangEnumShape {
    pub fn new(
        name: String,
        variants: Vec<TlangEnumVariant>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> Self {
        Self {
            name,
            variants,
            method_map: methods,
        }
    }

    pub fn get_method(&self, name: &str) -> Option<&TlangStructMethod> {
        self.method_map.get(name)
    }
}
