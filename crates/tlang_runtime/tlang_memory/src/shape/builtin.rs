use std::collections::HashMap;

use slab::Slab;

use crate::shape::TlangEnumVariant;

use super::{ShapeKey, TlangEnumShape, TlangShape, TlangStructShape};

pub struct BuiltinShapes {
    pub list: ShapeKey,
    pub list_iterator: ShapeKey,
    pub option: ShapeKey,
    pub result: ShapeKey,

    store: Slab<TlangShape>,
}

impl Default for BuiltinShapes {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinShapes {
    pub fn new() -> Self {
        let mut store = Slab::with_capacity(3);

        let option = Self::create_option_shape(&mut store);
        let result = Self::create_result_shape(&mut store);
        let list = Self::create_list_shape(&mut store);
        let list_iterator = Self::create_list_iterator_shape(&mut store);

        Self {
            list,
            list_iterator,
            option,
            result,
            store,
        }
    }

    fn create_option_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_enum_shape(
            "Option".to_string(),
            vec![
                TlangEnumVariant {
                    name: "Some".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
                TlangEnumVariant {
                    name: "None".to_string(),
                    field_map: HashMap::new(),
                },
            ],
            HashMap::new(),
        )))
    }

    fn create_result_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_enum_shape(
            "Result".to_string(),
            vec![
                TlangEnumVariant {
                    name: "Ok".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
                TlangEnumVariant {
                    name: "Err".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
            ],
            HashMap::new(),
        )))
    }

    fn create_list_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "List".to_string(),
            vec![],
            HashMap::new(),
        )))
    }

    fn create_list_iterator_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "ListIterator".to_string(),
            vec!["list".to_string(), "index".to_string()],
            HashMap::new(),
        )))
    }

    pub fn get_shape(&self, key: ShapeKey) -> Option<&TlangShape> {
        self.store.get(key.get_native_index())
    }

    pub fn get_shape_mut(&mut self, key: ShapeKey) -> Option<&mut TlangShape> {
        self.store.get_mut(key.get_native_index())
    }

    /// # Panics
    pub fn get_list_shape(&self) -> &TlangStructShape {
        self.get_shape(self.list)
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_shape_mut(&mut self) -> &mut TlangStructShape {
        self.get_shape_mut(self.list)
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_iterator_shape(&self) -> &TlangStructShape {
        self.get_shape(self.list_iterator)
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_iterator_shape_mut(&mut self) -> &mut TlangStructShape {
        self.get_shape_mut(self.list_iterator)
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_option_shape(&self) -> &TlangEnumShape {
        self.get_shape(self.option)
            .and_then(|s| s.get_enum_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_option_shape_mut(&mut self) -> &mut TlangEnumShape {
        self.get_shape_mut(self.option)
            .and_then(|s| s.get_enum_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_result_shape(&self) -> &TlangEnumShape {
        self.get_shape(self.result)
            .and_then(|s| s.get_enum_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_result_shape_mut(&mut self) -> &mut TlangEnumShape {
        self.get_shape_mut(self.result)
            .and_then(|s| s.get_enum_shape_mut())
            .unwrap()
    }
}
