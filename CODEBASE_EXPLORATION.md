# Tlang Codebase Exploration Report

## 1. HIR (High-level Intermediate Representation) Structure

**Location:** `crates/tlang_hir/src/hir.rs`

### Core HIR Nodes

#### **Module**
```rust
pub struct Module {
    pub hir_id: HirId,
    pub block: Block,
    pub span: Span,
}
```
Top-level container for the entire program. Contains a block with module-level statements.

#### **Block**
```rust
pub struct Block {
    pub hir_id: HirId,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,  // Optional completion expression
    scope: HirScopeData,      // locals/upvars count
    pub span: Span,
}
```
Represents a scope containing statements and an optional expression.

#### **Statement Kind (StmtKind)**
```rust
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(Box<Pat>, Box<Expr>, Box<Ty>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    DynFunctionDeclaration(Box<DynFunctionDeclaration>),
    Return(Option<Box<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
    ProtocolDeclaration(Box<ProtocolDeclaration>),
    ImplBlock(Box<ImplBlock>),
}
```

#### **Expression Kind (ExprKind)** - 24 variants
```rust
pub enum ExprKind {
    Block(Box<Block>),                           // Blocks can be expressions
    Loop(Box<Block>),                            // Infinite loops
    Break(Option<Box<Expr>>),                    // Break from loop
    Continue,                                     // Continue to next iteration
    Call(Box<CallExpression>),                   // Function/method call
    TailCall(Box<CallExpression>),               // Tail-recursive call (opt.)
    Cast(Box<Expr>, Box<Ty>),                    // Type cast
    Binary(BinaryOpKind, Box<Expr>, Box<Expr>), // Binary operations
    Unary(UnaryOp, Box<Expr>),                   // Unary operations (!x, -x, ...)
    Let(Box<Pat>, Box<Expr>),                    // Let pattern (only in if/guards)
    IfElse(Box<Expr>, Box<Block>, Vec<ElseClause>),  // if-else-if chains
    Path(Box<Path>),                             // Variable/function references
    List(Vec<Expr>),                             // List literals [1,2,3]
    Dict(Vec<(Expr, Expr)>),                     // Dict/object literals
    FunctionExpression(Box<FunctionDeclaration>), // Anonymous functions
    FieldAccess(Box<Expr>, Ident),               // obj.field
    IndexAccess(Box<Expr>, Box<Expr>),           // arr[idx]
    Literal(Box<Literal>),                       // 42, "string", true, etc
    Match(Box<Expr>, Vec<MatchArm>),             // Pattern matching
    Range(Box<RangeExpression>),                 // 1..10, 1..=10
    Wildcard,                                     // _ patterns
}
```

#### **Pattern Kind (PatKind)**
```rust
pub enum PatKind {
    Wildcard,                               // _
    Identifier(HirId, Box<Ident>),         // x (binding)
    Literal(Box<Literal>),                  // 42, "str"
    List(Vec<Pat>),                         // [x, y, z] or destructuring
    Rest(Box<Pat>),                         // ...rest
    Enum(Box<Path>, Vec<(Ident, Pat)>),    // Enum::Variant(x, y)
}
```

#### **Binary Operators (BinaryOpKind)** - 17 variants
```rust
pub enum BinaryOpKind {
    Assign, Add, Sub, Mul, Div, Mod, Exp,
    Eq, NotEq, Less, LessEq, Greater, GreaterEq,
    And, Or,
    BitwiseAnd, BitwiseOr, BitwiseXor,
}
```

#### **Match Arm**
```rust
pub struct MatchArm {
    pub hir_id: HirId,           // Scope for pattern bindings
    pub pat: Pat,                 // Pattern to match
    pub guard: Option<Expr>,      // Optional guard condition
    pub block: Block,             // Consequent block
    pub pat_locals: usize,        // # locals in pattern scope (populated by optimizer)
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
}
```

#### **Function Declaration**
```rust
pub struct FunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,              // Path expression for name
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}
```

#### **Type (Ty)**
```rust
pub enum TyKind {
    Unknown,
    Path(Path),                   // Named types
    Union(Vec<Path>),             // Union types
}
```

### Resolution and Slot Information

#### **Res (Resolution)** - Tracks symbol resolution
```rust
pub struct Res {
    hir_id: Option<HirId>,
    binding_kind: BindingKind,
    slot: Slot,
}

pub enum BindingKind {
    Local, Upvar, Temp, Struct, Enum, Variant, Fn, Param, Field, Closure, PrimTy, Unknown
}

pub enum Slot {
    Local(SlotIndex),                    // Local variable in current scope
    Upvar(SlotIndex, ScopeIndex),       // Upvar from parent scope
    Global(SlotIndex),                   // Global/builtin
    None,
}
```

---

## 2. HIR Optimizer Crate (`crates/tlang_hir_opt/src/`)

**File Structure:**
```
tlang_hir_opt/src/
├── lib.rs                      # Main exports
├── hir_opt.rs                  # Optimizer framework
├── constant_folding/
│   ├── mod.rs
│   ├── constant_folder.rs      # Constant folding pass
│   └── constant_propagation.rs # Constant propagation pass
├── symbol_resolution/
│   ├── mod.rs
│   └── identifier_resolver.rs  # Symbol resolution pass
└── slot_allocation/
    ├── mod.rs
    ├── slot_allocator.rs       # Variable slot allocation
    └── scope_data_updater.rs    # Updates scope metadata
```

### HirPass Trait (Optimizer Interface)
```rust
pub trait HirPass {
    fn name(&self) -> &'static str { std::any::type_name::<Self>() }
    fn init_context(&mut self, ctx: &mut HirOptContext) {}
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool;
}
```

**Returns:** `bool` indicating if modifications were made (enables iterative optimization)

### HirOptContext
```rust
pub struct HirOptContext {
    pub symbols: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub current_scope: HirId,
}
```

### Default Optimizer Passes (in order)
1. **SymbolResolution** (IdentifierResolver)
   - Resolves all identifier references to their definitions
   - Populates Res with hir_id and binding_kind

2. **ConstantFolding** (Group)
   - ConstantFolder: Evaluates constant expressions
   - ConstantPropagator: Propagates constant values

3. **SlotAllocation** (Group)
   - SlotAllocator: Assigns slot indices to all local variables
   - ScopeDataUpdater: Updates locals/upvars counts in scopes

### HirOptimizer
```rust
pub struct HirOptimizer(HirOptGroup);

impl Default for HirOptimizer {
    fn default() -> Self {
        Self::new(vec![
            Box::new(SymbolResolution::default()),
            Box::new(ConstantFolding::default()),
            Box::new(SlotAllocation::default()),
        ])
    }
}
```

**Note:** Passes run with a 10-iteration max limit to prevent infinite loops.

---

## 3. JS Codegen (`crates/tlang_codegen_js/src/`)

**File Structure:**
```
tlang_codegen_js/src/
├── lib.rs
├── generator.rs                  # Main CodegenJS struct
├── expr_generator.rs             # Expression code generation
├── stmt_generator.rs             # Statement code generation
├── pattern_match_generator.rs    # Match expression generation
├── function_generator.rs
├── binary_operator_generator.rs
├── enum_generator.rs
├── struct_generator.rs
├── scope.rs                      # Variable scope tracking
├── js_hir_opt.rs
└── js/
    ├── keywords.rs
    ├── utils.rs
    └── mod.rs
```

### Core Challenge: Expression/Statement Impedance Mismatch

**Problem:** JavaScript has fewer built-in expressions than Tlang. Control flow like if/else and match must become statements in JS, but HIR treats them as expressions.

**Solution:** Use completion variables and temp variables for statement-to-expression conversion.

### CodegenJS Main Structure
```rust
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    scopes: Scope,                           // Variable scoping
    context_stack: Vec<BlockContext>,        // Current context
    function_context_stack: Vec<FunctionContext>,
    match_context_stack: MatchContextStack,
    statement_buffer: Vec<String>,           // Deferred statements
    completion_variables: Vec<Option<Box<str>>>, // Temp var stack
    render_ternary: bool,                    // Can use ?: operator
}

pub enum BlockContext {
    Program,      // Top-level
    Statement,    // In statement position
    Expression,   // In expression position
}
```

### Completion Variables (Key Mechanism)

**Purpose:** When an expression with side-effects (if/else, match, block) is used in expression context, assign result to temp variable.

**Stack-based:** Each nested expression context pushes a completion variable.

**Examples:**
```
// "let a = { x + 1 }" becomes:
let a = (() => {
  return x + 1;
})();

// "x = if (cond) { 5 } else { 10 }" becomes:
let __tmp_0;
if (cond) {
  __tmp_0 = 5;
} else {
  __tmp_0 = 10;
}
x = __tmp_0;
```

### If/Else Expression Handling

#### Ternary Operator Optimization
```rust
pub(crate) fn should_render_if_else_as_ternary(
    &self,
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    self.get_render_ternary()
        && self.current_context() == BlockContext::Expression
        && else_branches.len() == 1  // No nested ternaries
        && if_else_can_render_as_ternary(expr, then_branch, else_branches)
}
```

#### Ternary Feasibility Check
```rust
fn if_else_can_render_as_ternary(
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    then_branch.stmts.is_empty()        // No statements
        && expr_can_render_as_js_expr(expr)
        && expr_can_render_as_js_expr(then_branch.expr.as_ref().unwrap())
        && else_branches.iter().all(|else_branch| {
            else_branch.consequence.stmts.is_empty()
                && (else_branch.condition.is_none()
                    || expr_can_render_as_js_expr(else_branch.condition.as_ref().unwrap()))
                && expr_can_render_as_js_expr(else_branch.consequence.expr.as_ref().unwrap())
        })
}
```

#### Pure Expression Detection
```rust
pub(crate) fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) => true,
        hir::ExprKind::Literal(..) => true,
        hir::ExprKind::Binary(_, lhs, rhs) => 
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs),
        hir::ExprKind::Call(call_expr) => 
            call_expr.arguments.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => 
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index),
        hir::ExprKind::List(exprs) => exprs.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::FunctionExpression(..) => true,
        // Control flow cannot render as pure JS expr:
        hir::ExprKind::Block(..) => false,
        hir::ExprKind::Loop(..) => false,
        hir::ExprKind::IfElse(..) => false,
        hir::ExprKind::Match(..) => false,
        hir::ExprKind::Break(..) => false,
        hir::ExprKind::Continue => false,
        _ => true,
    }
}
```

### Match Expression Handling - Multi-Stage Setup

#### Stage 1: Completion Variable Setup
```rust
fn setup_match_completion_variables(&mut self, arms: &[hir::MatchArm]) 
    -> (String, bool)  // (lhs context, has_let_seen)
{
    if has_block_completions {
        if self.can_reuse_current_completion_variable() {
            self.push_completion_variable(Some("return"));
        } else {
            let tmp = self.current_scope().declare_tmp_variable();
            self.push_let_declaration(&tmp);
            self.push_completion_variable(Some(&tmp));
        }
    }
}
```

#### Stage 2: Match Context Setup (Optimization)
```rust
fn setup_match_context(&mut self, expr: &hir::Expr) {
    match &expr.kind {
        hir::ExprKind::Path(path) => {
            // Direct identifier: no temp var needed
            self.match_context_stack
                .push(MatchContext::Identifier(identifier.clone()));
        }
        hir::ExprKind::List(exprs) if all_paths => {
            // List of identifiers: bind each
            self.match_context_stack
                .push(MatchContext::ListOfIdentifiers(...));
        }
        _ => {
            // Complex expr: create temp binding
            let tmp = self.current_scope().declare_tmp_variable();
            self.push_let_declaration_to_expr(&tmp, expr);
            self.match_context_stack.push(MatchContext::Dynamic);
        }
    }
}
```

**Optimization:** Avoids temp variable if match expr is already a simple identifier.

#### Stage 3: Pattern Identifier Setup
```rust
fn setup_pattern_identifiers(&mut self, arms: &[hir::MatchArm], has_let: &mut bool) {
    // Collect all pattern-bound identifiers across all arms
    // Declare them as let variables at the top
    for binding in all_pat_identifiers {
        let binding = self.current_scope().declare_variable(&binding);
        self.push_str(&binding);  // Add to let statement
    }
}
```

#### Temp Variable Generation Strategy
- **Temp naming:** `__tmp_0`, `__tmp_1`, etc. via `declare_tmp_variable()`
- **Scope tracking:** Each scope maintains a counter
- **Reuse:** Attempts to reuse "return" variable when possible
- **Per-match:** New temp for each match expression (currently no optimization across matches)

### Brittle/Buggy Mechanisms (from code comments)

1. **TODO in expr_generator.rs (line 366):**
   ```rust
   // TODO: Potentially in a return position or other expression, before we generate the if
   //       statement, replace the current statement buffer with a new one, generate the if
   //       statement, and then swap the statement buffer back.
   //       Similar to how we generate blocks in expression position.
   // TODO: Find a way to do this generically for all expressions which are represented as
   //       statements in JavaScript.
   // TODO: In case of recursive calls in tail position, we'll want to omit lhs.
   ```

2. **Pattern match generator (line 259):**
   ```rust
   Some(MatchContext::ListOfIdentifiers(_)) => String::new(), // ohoh...
   ```
   This indicates a potential unhandled case when match expr is a list of identifiers.

3. **Pattern match generator (line 326):**
   ```rust
   // TODO: Ugly workaround, as we always push a comma when generating pat identifiers
   //       bindings.
   if *has_let {
       self.push_char(',');
   } else {
       self.push_indent();
       self.push_str("let ");
       *has_let = true;
   }
   ```

4. **Block expression generation (line 37):**
   ```rust
   if has_completion_var {
       // Halp I made a mess
   } else {
       // Generate properly
   }
   ```

### Statement Lifting & Buffer Swapping

**Mechanism:** `statement_buffer` stack holds deferred JS statements.

```rust
pub(crate) fn generate_block_expression(&mut self, block: &hir::Block) {
    let completion_tmp_var = if block.has_completion() {
        self.current_completion_variable()
            .map(str::to_string)
            .unwrap_or_else(|| self.current_scope().declare_tmp_variable())
    } else {
        String::new()
    };

    // Swap: extract current statement buffer context
    let lhs = if has_completion_var {
        String::new()
    } else {
        self.replace_statement_buffer_with_empty_string()
    };

    // Generate block in isolated statement context
    self.generate_statements(&block.stmts);

    // Restore and inject results
    if !block.has_completion() {
        self.push_statement_buffer();
        self.push_str(&lhs);
        self.flush_statement_buffer();
        self.pop_statement_buffer();
    }
}
```

---

## 4. HIR Lowering/AST Lowering (`crates/tlang_ast_lowering/src/`)

**File Structure:**
```
tlang_ast_lowering/src/
├── lib.rs           # Main lowering entry point
├── expr.rs          # Expression lowering
├── stmt.rs          # Statement lowering
└── loop.rs          # Loop/for-loop special handling

tests/
└── common/mod.rs    # Test helpers
```

### Main Lowering Function
```rust
pub fn lower_to_hir(
    tlang_ast: &ast::node::Module,
    symbol_id_allocator: SymbolIdAllocator,
    root_symbol_table: Rc<RefCell<SymbolTable>>,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
) -> hir::LowerResult
```

**Output:** `(Module, LowerResultMeta)` where LowerResultMeta contains:
- `root_symbol_table: HirId`
- `symbol_tables: HashMap<HirId, Rc<RefCell<SymbolTable>>>`
- `hir_id_allocator: HirIdAllocator`
- `symbol_id_allocator: SymbolIdAllocator`

### LoweringContext
```rust
pub struct LoweringContext {
    node_id_to_hir_id: HashMap<NodeId, HirId>,    // AST -> HIR mapping
    fn_node_id_to_hir_id: HashMap<NodeId, HirId>, // Function-specific mapping
    hir_id_allocator: HirIdAllocator,
    symbol_id_allocator: SymbolIdAllocator,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    new_symbol_tables: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    current_symbol_table: Rc<RefCell<SymbolTable>>,
}
```

### Key Lowering Patterns

#### Scope Management
```rust
pub(crate) fn with_scope<F, R>(&mut self, node_id: NodeId, f: F) -> R
where F: FnOnce(&mut Self) -> R, R: hir::HirScope
{
    // Switch to scope for node_id, execute f, restore previous scope
    let previous = self.current_symbol_table.clone();
    self.current_symbol_table = self.symbol_tables.get(&node_id).cloned();
    let result = f(self);
    self.current_symbol_table = previous;
    result
}
```

#### New Scope Creation
```rust
pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
where F: FnOnce(&mut Self, Rc<RefCell<SymbolTable>>) -> (HirId, R), R: hir::HirScope
{
    // Create brand new scope (used for blocks without AST scope entry)
}
```

### Expression Lowering (expr.rs)

Maps AST expressions to HIR expressions. Notable transformations:

1. **ForLoop to FoldExpression:**
   ```rust
   pub(crate) fn lower_for_loop(&mut self, node_id: NodeId, for_loop: &ast::node::ForLoop) 
       -> hir::ExprKind
   ```
   Creates: iterator binding + iterator$$, accumulator$$ temporary bindings
   Generates: Iterable::iter() call, fold operation

2. **If-Let to Match:**
   ```rust
   ast::node::ExprKind::IfElse(if_else_expr) 
       if let ast::node::ExprKind::Let(pat, expr) = &condition.kind
   ```
   Transforms `if let pat = expr { ... }` into `match expr { pat => ... }`

3. **Direct Mappings:**
   - Loop → Block wrapped in Loop
   - Break/Continue → Direct mapping
   - Binary ops → BinaryOpKind
   - Call/TailCall → CallExpression wrapping
   - FieldAccess/IndexAccess → Direct mapping

### Test Helpers (tests/common/mod.rs)

```rust
pub fn hir_from_str(input: &str) -> tlang_hir::Module {
    let ast = parse_str(input);
    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        Default::default(),      // No semantic analysis
        Default::default(),
        Default::default(),
    );
    module
}

pub fn hir_from_str_analyzed(input: &str) -> tlang_hir::Module {
    let mut ast = parse_str(input);
    let mut analyzer = tlang_semantics::SemanticAnalyzer::default();
    let _ = analyzer.analyze(&mut ast);  // Semantic analysis first
    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    );
    module
}

pub fn pretty_print(node: &tlang_hir::Module) -> String {
    let mut printer = tlang_hir_pretty::HirPretty::new(
        tlang_hir_pretty::HirPrettyOptions {
            mark_unresolved: false,
            ..Default::default()
        }
    );
    printer.print_module(node);
    printer.output().to_string()
}
```

---

## 5. ANF (Administrative Normal Form) / Normalization Passes

**Finding:** No ANF passes found in codebase.

The HIR allows arbitrary nesting of expressions. The JS codegen instead uses:
- **Completion variables** for expression-to-statement conversion
- **Statement buffers** for deferred code generation
- **Scope tracking** for variable naming

This is a post-hoc transformation during code generation rather than a separate HIR normalization phase.

---

## Summary of Key Architectural Decisions

| Aspect | Implementation |
|--------|-----------------|
| **Expression/Statement Mismatch** | Completion variables + temp variable generation |
| **If-else as expressions** | Can render as ternary if both branches are pure expressions |
| **Match expressions** | Multi-stage setup with pattern identifier pre-declaration |
| **Temp variable naming** | Scope-level counter: `__tmp_0`, `__tmp_1`, etc. |
| **Statement deferral** | Stack-based statement buffers for nested contexts |
| **Loop optimization** | Tail recursion detection + unwrapping to while loops |
| **For-loop lowering** | Transformed to iterator + fold operations during AST→HIR |
| **Optimizer structure** | Pass-based with 10-iteration limit, composition via HirOptGroup |
| **No ANF pass** | Direct code generation with runtime statement deferral |
