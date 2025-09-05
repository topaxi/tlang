use tlang_symbols::{SymbolInfo, SymbolTable, SymbolType, SymbolIdAllocator};
use tlang_span::{Span, LineColumn};
use std::rc::Rc;
use std::cell::RefCell;

#[test]
fn test_direct_symbol_lookup_by_id() {
    let mut allocator = SymbolIdAllocator::default();
    let mut table = SymbolTable::new();
    
    // Create and insert symbols
    let id1 = allocator.next_id();
    let symbol1 = SymbolInfo::new(
        id1,
        "test_function",
        SymbolType::Function(1),
        Span::default(),
        LineColumn::default(),
    );
    
    let id2 = allocator.next_id();
    let symbol2 = SymbolInfo::new(
        id2,
        "test_variable",
        SymbolType::Variable,
        Span::default(),
        LineColumn::default(),
    );
    
    table.insert(symbol1.clone());
    table.insert(symbol2.clone());
    
    // Test direct lookup by ID
    let found1 = table.get_by_id(id1);
    assert!(found1.is_some());
    assert_eq!(found1.unwrap().name.as_ref(), "test_function");
    
    let found2 = table.get_by_id(id2);
    assert!(found2.is_some());
    assert_eq!(found2.unwrap().name.as_ref(), "test_variable");
    
    // Test lookup of non-existent ID
    let id3 = allocator.next_id();
    let found3 = table.get_by_id(id3);
    assert!(found3.is_none());
}

#[test]
fn test_hierarchical_symbol_lookup_by_id() {
    let mut allocator = SymbolIdAllocator::default();
    
    // Create parent table
    let parent_table = Rc::new(RefCell::new(SymbolTable::new()));
    
    let parent_id = allocator.next_id();
    let parent_symbol = SymbolInfo::new(
        parent_id,
        "parent_symbol",
        SymbolType::Function(0),
        Span::default(),
        LineColumn::default(),
    );
    
    parent_table.borrow_mut().insert(parent_symbol);
    
    // Create child table
    let child_table = SymbolTable::new_child(parent_table.clone());
    
    let child_id = allocator.next_id();
    let child_symbol = SymbolInfo::new(
        child_id,
        "child_symbol",
        SymbolType::Variable,
        Span::default(),
        LineColumn::default(),
    );
    
    let mut child_table = child_table;
    child_table.insert(child_symbol);
    
    // Child should find its own symbol
    let found_child = child_table.get_by_id(child_id);
    assert!(found_child.is_some());
    assert_eq!(found_child.unwrap().name.as_ref(), "child_symbol");
    
    // Child should find parent symbol (traversal)
    let found_parent = child_table.get_by_id(parent_id);
    assert!(found_parent.is_some());
    assert_eq!(found_parent.unwrap().name.as_ref(), "parent_symbol");
}

#[test]
fn test_new_storage_pattern_direct_access() {
    let mut allocator = SymbolIdAllocator::default();
    
    // Create table with new storage pattern
    let mut table = SymbolTable::new();
    
    // Insert symbols using new pattern
    let id1 = allocator.next_id();
    let symbol1 = SymbolInfo::new(
        id1,
        "storage_function",
        SymbolType::Function(2),
        Span::default(),
        LineColumn::default(),
    );
    
    let actual_id1 = table.insert(symbol1);
    
    // Test that we can find by the original ID (this is what semantic analyzer expects)
    let found_by_original = table.get_by_id(id1);
    assert!(found_by_original.is_some());
    assert_eq!(found_by_original.unwrap().name.as_ref(), "storage_function");
    
    // Test direct access (no traversal)
    let found = table.get_by_id(actual_id1);
    assert!(found.is_some());
    assert_eq!(found.unwrap().name.as_ref(), "storage_function");
    
    // The original and returned IDs should be the same
    assert_eq!(actual_id1, id1);
}

#[test]
fn test_storage_pattern_scope_boundaries() {
    let mut allocator = SymbolIdAllocator::default();
    
    // Create parent with storage
    let parent_table = Rc::new(RefCell::new(SymbolTable::new()));
    
    let parent_id = allocator.next_id();
    let parent_symbol = SymbolInfo::new(
        parent_id,
        "parent_function",
        SymbolType::Function(0),
        Span::default(),
        LineColumn::default(),
    );
    
    let actual_parent_id = parent_table.borrow_mut().insert(parent_symbol);
    
    // Create child with shared storage
    let child_table = Rc::new(RefCell::new(SymbolTable::new_child(parent_table.clone())));
    
    let child_id = allocator.next_id();
    let child_symbol = SymbolInfo::new(
        child_id,
        "child_parameter",
        SymbolType::Parameter,
        Span::default(),
        LineColumn::default(),
    );
    
    let actual_child_id = child_table.borrow_mut().insert(child_symbol);
    
    // Child can see parent (upward scope access)
    let child_sees_parent = child_table.borrow().get_by_id(actual_parent_id);
    assert!(child_sees_parent.is_some());
    assert_eq!(child_sees_parent.unwrap().name.as_ref(), "parent_function");
    
    // Child can see parent using original ID too
    let child_sees_parent_orig = child_table.borrow().get_by_id(parent_id);
    assert!(child_sees_parent_orig.is_some());
    assert_eq!(child_sees_parent_orig.unwrap().name.as_ref(), "parent_function");
    
    // Child can see itself
    let child_sees_self = child_table.borrow().get_by_id(actual_child_id);
    assert!(child_sees_self.is_some());
    assert_eq!(child_sees_self.unwrap().name.as_ref(), "child_parameter");
    
    // Child can see itself using original ID too
    let child_sees_self_orig = child_table.borrow().get_by_id(child_id);
    assert!(child_sees_self_orig.is_some());
    assert_eq!(child_sees_self_orig.unwrap().name.as_ref(), "child_parameter");
    
    // Parent cannot see child (scope boundary enforcement)
    let parent_sees_child = parent_table.borrow().get_by_id(actual_child_id);
    assert!(parent_sees_child.is_none());
}