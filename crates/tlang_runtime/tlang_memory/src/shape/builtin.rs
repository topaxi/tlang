use std::collections::HashMap;

use slab::Slab;

use crate::shape::TlangEnumVariant;

use super::{ShapeKey, TlangEnumShape, TlangShape, TlangStructShape};

pub struct BuiltinShapes {
    pub list: ShapeKey,
    pub list_iterator: ShapeKey,
    pub option: ShapeKey,
    pub result: ShapeKey,
    pub regex: ShapeKey,
    pub string_buf: ShapeKey,
    pub string: ShapeKey,
    pub slice: ShapeKey,

    store: Slab<TlangShape>,
    shapes_by_name: HashMap<String, ShapeKey>,
}

impl Default for BuiltinShapes {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinShapes {
    pub fn new() -> Self {
        let mut store = Slab::with_capacity(8);

        let option = Self::create_option_shape(&mut store);
        let result = Self::create_result_shape(&mut store);
        let list = Self::create_list_shape(&mut store);
        let list_iterator = Self::create_list_iterator_shape(&mut store);
        let regex = Self::create_regex_shape(&mut store);
        let string_buf = Self::create_string_buf_shape(&mut store);
        let string = Self::create_string_shape(&mut store);
        let slice = Self::create_slice_shape(&mut store);

        let mut shapes_by_name = HashMap::with_capacity(8);
        shapes_by_name.insert("Option".to_string(), option);
        shapes_by_name.insert("Result".to_string(), result);
        shapes_by_name.insert("List".to_string(), list);
        shapes_by_name.insert("ListIterator".to_string(), list_iterator);
        shapes_by_name.insert("Regex".to_string(), regex);
        shapes_by_name.insert("StringBuf".to_string(), string_buf);
        shapes_by_name.insert("String".to_string(), string);
        shapes_by_name.insert("Slice".to_string(), slice);

        Self {
            list,
            list_iterator,
            option,
            result,
            regex,
            string_buf,
            string,
            slice,
            store,
            shapes_by_name,
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

    fn create_regex_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "Regex".to_string(),
            vec![],
            HashMap::new(),
        )))
    }

    fn create_string_buf_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "StringBuf".to_string(),
            vec![],
            HashMap::new(),
        )))
    }

    fn create_string_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "String".to_string(),
            vec![],
            HashMap::new(),
        )))
    }

    fn create_slice_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "Slice".to_string(),
            vec![],
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

    /// # Panics
    pub fn get_regex_shape(&self) -> &TlangStructShape {
        self.get_shape(self.regex)
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_regex_shape_mut(&mut self) -> &mut TlangStructShape {
        self.get_shape_mut(self.regex)
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_string_buf_shape(&self) -> &TlangStructShape {
        self.get_shape(self.string_buf)
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_string_buf_shape_mut(&mut self) -> &mut TlangStructShape {
        self.get_shape_mut(self.string_buf)
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    // ── Name-based lookup & registration ────────────────────────────────

    /// Look up a shape by its type name (e.g. "Option", "List").
    pub fn lookup(&self, name: &str) -> Option<ShapeKey> {
        self.shapes_by_name.get(name).copied()
    }

    /// Register a shape under a name, making it discoverable via `lookup`.
    pub fn register(&mut self, key: ShapeKey, name: &str) {
        self.shapes_by_name.insert(name.to_string(), key);
    }

    /// Insert a new struct shape into the store and register it by name.
    pub fn insert_struct_shape(&mut self, name: String, fields: Vec<String>) -> ShapeKey {
        let key = ShapeKey::new_native(self.store.insert(TlangShape::new_struct_shape(
            name.clone(),
            fields,
            HashMap::new(),
        )));
        self.shapes_by_name.insert(name, key);
        key
    }

    /// Insert a new enum shape into the store and register it by name.
    pub fn insert_enum_shape(&mut self, name: String, variants: Vec<TlangEnumVariant>) -> ShapeKey {
        let key = ShapeKey::new_native(self.store.insert(TlangShape::new_enum_shape(
            name.clone(),
            variants,
            HashMap::new(),
        )));
        self.shapes_by_name.insert(name, key);
        key
    }
}
