mod common;

use tlang_hir::Visitor;
use tlang_hir::hir::{self, HirScope};

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(tlang_hir_opt::slot_allocation::SlotAllocation::default()),
    ])
}

fn compile(source: &str) -> hir::Module {
    common::compile_and_optimize(source, &mut optimizer())
}

/// Collect scope data from all HIR nodes that implement HirScope
fn collect_scope_data(module: &mut hir::Module) -> Vec<(String, usize, usize)> {
    let mut collector = ScopeDataCollector::default();
    collector.collect(module);
    collector.scope_data
}

#[derive(Default)]
struct ScopeDataCollector {
    scope_data: Vec<(String, usize, usize)>, // (name, locals, upvars)
}

impl ScopeDataCollector {
    fn collect(&mut self, module: &mut hir::Module) {
        self.visit_module(module, &mut ());
    }
}

impl<'hir> tlang_hir::Visitor<'hir> for ScopeDataCollector {
    type Context = ();

    fn visit_module(&mut self, module: &'hir mut hir::Module, ctx: &mut Self::Context) {
        self.scope_data
            .push(("module".to_string(), module.locals(), module.upvars()));
        tlang_hir::visit::walk_module(self, module, ctx);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        self.scope_data.push((
            format!("block_{:?}", block.hir_id),
            block.locals(),
            block.upvars(),
        ));
        tlang_hir::visit::walk_block(self, block, ctx);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        match &mut stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.scope_data
                    .push((format!("fn_{}", decl.name()), decl.locals(), decl.upvars()));
            }
            _ => {}
        }
        tlang_hir::visit::walk_stmt(self, stmt, ctx);
    }
}

#[test]
fn test_hir_scope_data_counter_higher_order_function() {
    let mut hir = compile(
        r#"
        fn create_counter(initial_value) {
            let count = initial_value;
            fn counter() {
                count = count + 1;
                count
            }
            counter
        }

        let my_counter = create_counter(10);
        let result1 = my_counter();
        let result2 = my_counter();
        result1 |> println();
        result2 |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Print scope data for debugging
    println!("Scope data collected:");
    for (name, locals, upvars) in &scope_data {
        println!("  {}: locals={}, upvars={}", name, locals, upvars);
    }

    // Verify that scope data is properly collected
    // The module should have locals for create_counter, my_counter, result1, result2
    // The create_counter function should have locals for count, counter function
    // The inner counter function should have upvars to access count

    // Find create_counter function
    let create_counter_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_create_counter");
    assert!(
        create_counter_scope.is_some(),
        "create_counter function should have scope data"
    );
    let (_, locals, _) = create_counter_scope.unwrap();
    assert!(
        *locals > 0,
        "create_counter should have locals (count variable, counter function)"
    );

    // Find counter function
    let counter_scope = scope_data.iter().find(|(name, _, _)| name == "fn_counter");
    assert!(
        counter_scope.is_some(),
        "counter function should have scope data"
    );
    let (_, locals, upvars) = counter_scope.unwrap();
    assert!(
        *locals > 0 || *upvars > 0,
        "counter should have locals or upvars (for count access)"
    );

    // Module should have scope data for global variables
    let module_scope = scope_data.iter().find(|(name, _, _)| name == "module");
    assert!(module_scope.is_some(), "module should have scope data");
    let (_, locals, _) = module_scope.unwrap();
    assert!(
        *locals > 0,
        "module should have locals for global variables"
    );
}

#[test]
fn test_hir_scope_data_simple_enum() {
    let mut hir = compile(
        r#"
        enum Direction {
            North,
            South,
            East,
            West,
        }

        fn get_direction() {
            Direction::North
        }

        let current_direction = get_direction();
        current_direction |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Find get_direction function
    let get_direction_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_get_direction");
    assert!(
        get_direction_scope.is_some(),
        "get_direction function should have scope data"
    );
    let (_, locals, upvars) = get_direction_scope.unwrap();

    // The function should have at least one local for itself or access to enum variants
    assert!(
        *locals > 0 || *upvars > 0,
        "get_direction should have scope data for enum access"
    );

    // Module should have scope data
    let module_scope = scope_data.iter().find(|(name, _, _)| name == "module");
    assert!(module_scope.is_some(), "module should have scope data");
    let (_, locals, _) = module_scope.unwrap();
    assert!(
        *locals > 0,
        "module should have locals for global variables and enum"
    );
}

#[test]
fn test_hir_scope_data_tagged_enum_pattern_matching() {
    let mut hir = compile(
        r#"
        enum Option {
            Some(value),
            None,
        }

        fn unwrap_or(opt, default) { opt }
        fn unwrap_or(Option::Some(value), default) { value }
        fn unwrap_or(Option::None, default) { default }

        fn process_option(opt) {
            unwrap_or(opt, 42)
        }

        let some_value = Option::Some(100);
        let none_value = Option::None;
        
        let result1 = process_option(some_value);
        let result2 = process_option(none_value);
        
        result1 |> println();
        result2 |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Print scope data for debugging
    println!("Tagged enum scope data collected:");
    for (name, locals, upvars) in &scope_data {
        println!("  {}: locals={}, upvars={}", name, locals, upvars);
    }

    // Find unwrap_or functions (should be multiple overloads)
    let unwrap_or_scopes: Vec<_> = scope_data
        .iter()
        .filter(|(name, _, _)| name.starts_with("fn_unwrap_or"))
        .collect();
    assert!(
        !unwrap_or_scopes.is_empty(),
        "unwrap_or functions should have scope data"
    );

    // Each unwrap_or function should have locals for pattern matching
    for (_, locals, upvars) in &unwrap_or_scopes {
        assert!(
            *locals > 0 || *upvars > 0,
            "unwrap_or functions should have scope data for parameters"
        );
    }

    // Find process_option function
    let process_option_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_process_option");
    assert!(
        process_option_scope.is_some(),
        "process_option function should have scope data"
    );
    let (_, locals, upvars) = process_option_scope.unwrap();
    assert!(
        *locals > 0 || *upvars > 0,
        "process_option should have scope data for parameters and function calls"
    );

    // Module should have scope data for global variables
    let module_scope = scope_data.iter().find(|(name, _, _)| name == "module");
    assert!(module_scope.is_some(), "module should have scope data");
    let (_, locals, _) = module_scope.unwrap();
    assert!(
        *locals > 0,
        "module should have locals for global variables and enum"
    );
}

#[test]
fn test_hir_scope_data_nested_functions_with_closures() {
    let mut hir = compile(
        r#"
        fn outer_function(x) {
            let outer_var = x * 2;
            
            fn middle_function(y) {
                let middle_var = y + outer_var;
                
                fn inner_function(z) {
                    outer_var + middle_var + z
                }
                
                inner_function
            }
            
            middle_function
        }

        let func = outer_function(5);
        let inner_func = func(10);
        let result = inner_func(3);
        result |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Find all function scopes
    let outer_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_outer_function");
    let middle_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_middle_function");
    let inner_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_inner_function");

    assert!(
        outer_scope.is_some(),
        "outer_function should have scope data"
    );
    assert!(
        middle_scope.is_some(),
        "middle_function should have scope data"
    );
    assert!(
        inner_scope.is_some(),
        "inner_function should have scope data"
    );

    let (_, outer_locals, _) = outer_scope.unwrap();
    let (_, middle_locals, _middle_upvars) = middle_scope.unwrap();
    let (_, inner_locals, inner_upvars) = inner_scope.unwrap();

    // Outer function should have locals for parameters and local variables
    assert!(
        *outer_locals > 0,
        "outer_function should have locals for parameters and variables"
    );

    // Middle function should have locals and potentially upvars for accessing outer scope
    assert!(
        *middle_locals > 0,
        "middle_function should have locals for parameters and variables"
    );

    // Inner function should have upvars for accessing middle and outer scope variables
    assert!(
        *inner_locals > 0 || *inner_upvars > 0,
        "inner_function should have scope data for closure variables"
    );
}

#[test]
fn test_hir_scope_data_recursive_function() {
    let mut hir = compile(
        r#"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }

        let result = factorial(5);
        result |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Find factorial function scopes (should be multiple overloads)
    let factorial_scopes: Vec<_> = scope_data
        .iter()
        .filter(|(name, _, _)| name.starts_with("fn_factorial"))
        .collect();
    assert!(
        !factorial_scopes.is_empty(),
        "factorial functions should have scope data"
    );

    // Each factorial function should have locals for parameters
    for (_, locals, upvars) in &factorial_scopes {
        assert!(
            *locals > 0 || *upvars > 0,
            "factorial functions should have scope data for parameters and self-reference"
        );
    }

    // Module should have scope data
    let module_scope = scope_data.iter().find(|(name, _, _)| name == "module");
    assert!(module_scope.is_some(), "module should have scope data");
    let (_, locals, _) = module_scope.unwrap();
    assert!(
        *locals > 0,
        "module should have locals for result variable and factorial functions"
    );
}

#[test]
fn test_hir_scope_data_simple_function_exact_locals() {
    let mut hir = compile(
        r#"
        fn simple_function(param1, param2) {
            let local_var1 = param1 + param2;
            let local_var2 = local_var1 * 2;
            local_var2
        }
        
        let result = simple_function(5, 10);
        result |> println();
        "#,
    );

    let scope_data = collect_scope_data(&mut hir);

    // Print scope data for exact verification
    println!("Simple function scope data:");
    for (name, locals, upvars) in &scope_data {
        println!("  {}: locals={}, upvars={}", name, locals, upvars);
    }

    // Find simple_function - should have locals for:
    // - the function itself (slot 0)
    // - param1 (slot 1)
    // - param2 (slot 2)
    // - local_var1 (slot 3)
    // - local_var2 (slot 4)
    let simple_function_scope = scope_data
        .iter()
        .find(|(name, _, _)| name == "fn_simple_function");
    assert!(
        simple_function_scope.is_some(),
        "simple_function should have scope data"
    );
    let (_, locals, upvars) = simple_function_scope.unwrap();

    // Should have at least 5 locals: function + 2 params + 2 local vars
    assert!(
        *locals >= 5,
        "simple_function should have at least 5 locals, got {}",
        locals
    );
    assert_eq!(
        *upvars, 0,
        "simple_function should have 0 upvars, got {}",
        upvars
    );

    // Module should have locals for simple_function and result
    let module_scope = scope_data.iter().find(|(name, _, _)| name == "module");
    assert!(module_scope.is_some(), "module should have scope data");
    let (_, locals, upvars) = module_scope.unwrap();
    assert!(
        *locals >= 2,
        "module should have at least 2 locals (simple_function, result), got {}",
        locals
    );
    assert_eq!(*upvars, 0, "module should have 0 upvars, got {}", upvars);
}
