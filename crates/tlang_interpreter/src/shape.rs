use std::collections::HashMap;
use std::hash::DefaultHasher;

use tlang_hir::hir::HirId;

use crate::value::{TlangObjectId, TlangValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ShapeKeyImpl {
    HirId(HirId),
    Native(usize),
    // ShapeKeyImpl::Dict is a hash value generated from each of the keys in the struct.
    Dict(u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShapeKey(ShapeKeyImpl);

impl ShapeKey {
    pub fn new_native() -> Self {
        static NEXT_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        ShapeKey(ShapeKeyImpl::Native(
            NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        ))
    }

    pub fn new_hir_id(id: HirId) -> Self {
        ShapeKey(ShapeKeyImpl::HirId(id))
    }

    pub fn from_dict_keys(keys: &[String]) -> Self {
        let mut hasher = DefaultHasher::new();

        for key in keys.iter() {
            std::hash::Hash::hash(&key, &mut hasher);
        }

        let hash = std::hash::Hasher::finish(&hasher);

        ShapeKey(ShapeKeyImpl::Dict(hash))
    }
}

impl From<HirId> for ShapeKey {
    fn from(id: HirId) -> Self {
        ShapeKey::new_hir_id(id)
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
}
