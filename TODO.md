## Create separate semantic analysis step/crate

1. Create a way to bind semantic information to AST nodes
  - AnnotatedNode containing the Node and information
  - Keep enum but embed structs instead of the enum recursively
1. Create SymbolTable struct, containing link to parent plus hashmap of symbols
1. Refactor scope analysis into semantic analysis step and reuse data in codegen

### Random notes

```rust
pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
    parent: Option<Rc<RefCell<SymbolTable>>>,
}

pub struct AnnotatedNode {
    node: Node,
    symbol_table: Rc<RefCell<SymbolTable>>,
    // ...
}
```

or

```rust
#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    name: String,
    value: Box<Node>,
    symbol_table: Rc<RefCell<SymbolTable>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    name: String,
    parameters: Vec<Node>,
    body: Box<Node>,
    symbol_table: Rc<RefCell<SymbolTable>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    // ...
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    // ...
}
```

or

```rust
#[derive(Debug, PartialEq, Clone)]
pub struct AnnotatedNode {
    symbol_table: Rc<RefCell<SymbolTable>>,
    // Add other common fields here...
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    name: String,
    value: Box<Node>,
    node_data: AnnotatedNode,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    name: String,
    parameters: Vec<Node>,
    body: Box<Node>,
    node_data: AnnotatedNode,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    // ...
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    // ...
}
```
