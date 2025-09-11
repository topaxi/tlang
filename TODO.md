# Simplify HIR-to-JavaScript Expression Transformation 

## Current Problem Analysis

The current transformation framework (~2,500 lines across 3 files) is overly complex for what should be a simple task: converting expressions that cannot be represented in JavaScript into statements with temporary variables, similar to Administrative Normal Form (ANF).

### Issues with Current Approach:
1. **Over-engineered architecture**: Multiple nested transformers, complex hoisting logic, intricate assignment chains
2. **Counter synchronization problems**: Nested transformations create temp variable name collisions (`$hir$0` used by both outer and inner transformations)
3. **Complex scoping issues**: Variables created inside loops referenced outside their scope (`ReferenceError: $hir$X is not defined`)
4. **Duplicate declarations**: Same temp variables declared multiple times (`let $hir$1: unknown = _;`)
5. **Self-assignments**: Generated code like `$hir$X = $hir$X` 
6. **Unnecessary complexity**: Current pre-order transformation with nested strategy creation is too complex for basic expression flattening

### Test Evidence:
- Current failures include extra statements like `$hir$1;` appearing in generated code
- Incorrect temp variable assignments (`$hir$3 = $hir$1` instead of `$hir$0 = $hir$1`)
- Many tests failing due to architectural issues rather than logic errors

## Proposed Simple ANF-Like Approach

### Core Principle: Single-Pass Post-Order Transformation

Replace the current complex pre-order transformation framework with a simple single-pass post-order visitor that converts non-JavaScript-expressible constructs into statements with temp variables.

### Key Simplifications:

#### 1. **Eliminate Nested Transformer Creation**
- **Current**: Creates nested `ExpressionTransformer` instances causing counter sync issues
- **New**: Single transformer instance processes entire HIR tree in post-order
- **Benefit**: No counter synchronization problems, simpler temp variable management

#### 2. **Post-Order Traversal**
- **Current**: Pre-order transformation creates complex parent-child dependencies
- **New**: Process children first, then parent (standard ANF approach)
- **Benefit**: Inner expressions fully resolved before outer expressions processed

#### 3. **Simple Expression Classification**
```rust
enum ExpressionCategory {
    JavaScriptExpression,     // Can be directly translated to JS
    RequiresStatement,        // Needs to become a statement with temp variable
}
```

#### 4. **Systematic Temp Variable Strategy**
- **Current**: Complex hoisting logic, multiple declaration sites
- **New**: All temp variables declared at function scope level
- **Benefit**: No scoping issues, no duplicate declarations

## Implementation Plan

### Phase 1: Create New Simple Transformer (Target: ~200-300 lines total)

#### 1.1 New File Structure
```
crates/tlang_codegen_js/src/
├── anf_transformer.rs           # New simple ANF-like transformer (~200 lines)
├── expression_classifier.rs     # Simple classification logic (~100 lines)
└── temp_variable_manager.rs     # Simplified temp var management (~50 lines)
```

#### 1.2 Core ANF Transformer Interface
```rust
pub struct AnfTransformer {
    temp_counter: usize,
    pending_statements: Vec<hir::Stmt>,
}

impl AnfTransformer {
    pub fn transform_module(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) {
        // Single entry point for all transformations
    }
    
    fn transform_expr(&mut self, expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        // Post-order transformation of expressions
    }
}
```

#### 1.3 Simple Expression Classification
```rust
fn requires_statement_form(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Loop(_) => true,
        hir::ExprKind::Match(..) => true,
        hir::ExprKind::If(..) => has_complex_branches(expr),
        hir::ExprKind::Block(_) => true,  // Blocks always need statement form
        _ => false,
    }
}
```

**Note on Block Expressions**: All block expressions require statement form, even without completion expressions. For example:
```rust
// Input tlang:
let foo = bar || {
  let msg = "bar is falsy";
  panic(msg);
};

// Output JavaScript should be equivalent to:
if (!bar) {
  let msg = "bar is falsy";
  panic(msg);
}
let foo = bar;
```

### Phase 2: Implement Core Transformation Logic

#### 2.1 Loop Expression Handling
```rust
fn transform_loop_expr(&mut self, loop_expr: hir::Expr) -> hir::Expr {
    // 1. Generate temp variable if necessary
    // 2. Transform loop body (ensuring break statements assign to temp var)
    // 3. Add loop as statement to pending_statements
    // 4. Return temp variable reference
}
```

#### 2.2 Match Expression Handling  
```rust
fn transform_match_expr(&mut self, match_expr: hir::Expr) -> hir::Expr {
    // 1. Generate temp variable if necessary
    // 2. Transform each match arm to assign result to temp var
    // 3. Add match as statement to pending_statements  
    // 4. Return temp variable reference
}
```

#### 2.3 Block Expression Handling
```rust
fn transform_block_expr(&mut self, block_expr: hir::Expr) -> hir::Expr {
    // 1. Transform all statements in block
    // 2. If block has completion expression, extract it
    // 3. Generate temp variable for completion value if needed
    // 4. Return appropriate expression
}
```

### Phase 3: Integration and Testing

#### 3.1 Replace Current Implementation
```rust
// In hir_js_opt_group.rs
pub fn create_hir_js_opt_group() -> HirPassGroup {
    HirPassGroup::new(vec![
        Box::new(ReturnStatementPass),
        Box::new(AnfTransformer::new()),  // Replace RefactoredHirJsPass
    ])
}
```

#### 3.2 Validation Strategy
1. **Test compatibility**: All existing tests should pass
2. **Output comparison**: Generated JavaScript should be functionally equivalent
3. **Simplicity verification**: New implementation should be <500 lines total
4. **Performance check**: No significant performance regression

### Phase 4: Cleanup and Documentation

#### 4.1 Remove Old Code
- Delete `refactored_hir_js_pass.rs`
- Delete `transformation_strategies.rs`  
- Delete `expression_transformer.rs`
- Clean up exports in `lib.rs`

#### 4.2 Update Documentation
- Update copilot instructions with ANF approach guidelines
- Document the simplified transformation process
- Add examples of ANF transformation patterns

## Expected Benefits

### 1. **Dramatic Code Reduction**
- **Before**: ~2,500 lines across multiple files
- **After**: ~350 lines in focused files
- **Reduction**: ~85% code reduction

### 2. **Eliminated Complexity Issues**
- No more nested transformer creation
- No counter synchronization problems
- No complex hoisting logic
- No duplicate declarations
- No self-assignments

### 3. **Clearer Architecture**
- Single responsibility: Convert non-JS expressions to statements
- Predictable transformation order (post-order)
- Simple temp variable management
- Easy to understand and maintain

### 4. **Better Test Reliability**
- Fewer architectural edge cases
- More predictable output
- Easier debugging when issues arise

## Implementation Timeline

### Week 1: Foundation
- [ ] Create new ANF transformer structure
- [ ] Implement expression classification logic
- [ ] Create simplified temp variable manager

### Week 2: Core Transformations
- [ ] Implement loop expression transformation
- [ ] Implement match expression transformation  
- [ ] Implement block expression transformation
- [ ] Implement if-else expression transformation

### Week 3: Integration and Testing
- [ ] Replace old implementation in HIR pass group
- [ ] Run full test suite and fix any regressions
- [ ] Compare generated JavaScript output for equivalence

### Week 4: Cleanup
- [ ] Remove old transformation code
- [ ] Update documentation and guidelines
- [ ] Final testing and validation

## Success Criteria

1. **All existing tests pass** - No behavioral changes
2. **Code size reduction** - <500 lines total (from 2,500)
3. **No architectural issues** - No temp variable collisions, scoping issues, or duplicate declarations
4. **Maintainable code** - Easy to understand and modify
5. **Performance maintained** - No significant regression in compilation speed

## Technical Notes

### ANF Transformation Pattern
Administrative Normal Form ensures that:
- Every function argument is a variable or constant
- Every operator operand is a variable or constant  
- Complex expressions are broken into simple assignments
- Each intermediate result has its own temporary variable

### JavaScript Expression Compatibility
Expressions that can be directly translated to JavaScript:
- Variable references
- Literals (numbers, strings, booleans)
- Simple binary operations
- Function calls with simple arguments
- Array/object literals with simple elements

Expressions that require statement form:
- Loops (for, while, loop)
- Match expressions
- If-else with complex branches
- Blocks with completion expressions
- Break/continue statements

### Temp Variable Naming Strategy
- Use consistent `$hir$N` naming pattern
- Declare all temp variables at function scope to avoid scoping issues
- Use simple sequential counter (no complex nesting considerations)

### Transformation Order
Post-order traversal ensures:
1. All child expressions are fully transformed first
2. Parent expressions receive already-processed children
3. No need for complex recursive transformation coordination
4. Simpler reasoning about transformation state

This approach follows established compiler design patterns and should result in a much simpler, more maintainable transformation system while preserving all existing functionality.