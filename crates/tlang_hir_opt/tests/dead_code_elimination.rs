use insta::assert_snapshot;

mod common;

/// Optimizer with only symbol resolution + DCE (no constant folding/propagation)
/// so we can test DCE behavior in isolation.
fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(tlang_hir_opt::DeadCodeElimination::default()),
    ])
}

/// Optimizer with constant folding + DCE to test chained elimination.
fn optimizer_with_const_folding() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(tlang_hir_opt::constant_folding::ConstantFolding::default()),
        Box::new(tlang_hir_opt::DeadCodeElimination::default()),
    ])
}

// ---- Unused let bindings ----

#[test]
fn removes_unused_let_binding() {
    let source = r#"
        let x = 42;
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"");
}

#[test]
fn keeps_used_let_binding() {
    let source = r#"
        let x = 42;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 42;
    println(x);
    "###);
}

#[test]
fn removes_unused_keeps_used() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        println(y);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let y: unknown = 2;
    println(y);
    "###);
}

// ---- Constant folding + DCE chaining ----

#[test]
fn const_folding_chains_with_dce() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        println(y);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer_with_const_folding());
    assert_snapshot!(common::pretty_print(&hir), @"println(2);");
}

// ---- Side-effectful let bindings ----

#[test]
fn keeps_unused_let_with_call_rhs() {
    let source = r#"
        let x = println(1);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = println(1);
    "###);
}

// ---- Unused function declarations ----

#[test]
fn removes_unused_function() {
    let source = r#"
        fn foo() { 42 }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"");
}

#[test]
fn keeps_called_function() {
    let source = r#"
        fn foo() { 42 }
        println(foo());
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn foo() -> unknown {
        42
    }
    println(foo());
    "###);
}

// ---- Top-level expression statements ----

#[test]
fn keeps_top_level_call() {
    let source = r#"
        println(42);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    println(42);
    "###);
}

#[test]
fn removes_top_level_pure_expr() {
    let source = r#"
        1 + 2;
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"");
}

// ---- Nested dead code ----

#[test]
fn removes_dead_code_inside_functions() {
    let source = r#"
        fn foo() {
            let unused = 99;
            let used = 42;
            println(used);
        }
        foo();
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn foo() -> unknown {
        let used: unknown = 42;
        println(used);
    }
    foo();
    "###);
}

// ---- Enum declarations ----

#[test]
fn removes_unused_enum() {
    let source = r#"
        enum Color { Red, Green, Blue }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"");
}

#[test]
fn keeps_used_enum() {
    let source = r#"
        enum Color { Red, Green, Blue }
        let c = Color::Red;
        println(c);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    enum Color {
        Red
        Green
        Blue
    }
    let c: unknown = Color::Red;
    println(c);
    "###);
}

// ---- Struct declarations ----

#[test]
fn removes_unused_struct() {
    let source = r#"
        struct Point { x: int, y: int }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"");
}

#[test]
fn keeps_used_struct() {
    let source = r#"
        struct Point { x: int, y: int }
        let p = Point { x: 1, y: 2 };
        println(p);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    struct Point {
        x: int,
        y: int,
    }

    let p: unknown = Point({x: 1, y: 2});
    println(p);
    "###);
}

// ---- Completion expression in function body ----

#[test]
fn preserves_function_completion_expr() {
    let source = r#"
        fn foo() {
            let unused = 1;
            42
        }
        println(foo());
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn foo() -> unknown {
        42
    }
    println(foo());
    "###);
}

// ---- Assignment side effect ----

#[test]
fn keeps_assignment_side_effect() {
    let source = r#"
        let x = 1;
        x = 2;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 1;
    (x = 2);
    println(x);
    "###);
}

// ---- Multiple functions, only some used ----

#[test]
fn removes_only_unused_functions() {
    let source = r#"
        fn used_fn() { 1 }
        fn unused_fn() { 2 }
        println(used_fn());
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn used_fn() -> unknown {
        1
    }
    println(used_fn());
    "###);
}

// ---- Multi-clause (dyn dispatch) function DCE ----

#[test]
fn keeps_dyn_fn_group_when_arity_call_is_used() {
    // A direct arity-matched call resolves to the variant FunctionDeclaration, not
    // the DynFunctionDeclaration.  DCE must keep the whole group alive so that
    // the DynFn dispatch node and all variants retain their correct slots.
    let source = r#"
        fn add(a, b) { a + b }
        fn add(a) { add(a, 1) }
        println(add(5));
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    let pretty = common::pretty_print(&hir);
    // Both variants and the dyn dispatch wrapper must survive.
    assert!(pretty.contains("fn add/2"), "add/2 missing:\n{pretty}");
    assert!(pretty.contains("fn add/1"), "add/1 missing:\n{pretty}");
    // The call site must still exist.
    assert!(
        pretty.contains("println"),
        "println call missing:\n{pretty}"
    );
}

#[test]
fn keeps_dyn_fn_group_when_reassigned_and_called() {
    // When a multi-clause function is assigned to a variable the plain-name
    // path resolves to the DynFunctionDeclaration.  DCE must keep the whole
    // group alive so the dynamic dispatch still works at runtime.
    let source = r#"
        fn add(a, b) { a + b }
        fn add(a) { add(a, 1) }
        let f = add;
        println(f(5));
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    let pretty = common::pretty_print(&hir);
    assert!(pretty.contains("fn add/2"), "add/2 missing:\n{pretty}");
    assert!(pretty.contains("fn add/1"), "add/1 missing:\n{pretty}");
    assert!(pretty.contains("let f"), "let f missing:\n{pretty}");
    assert!(
        pretty.contains("println"),
        "println call missing:\n{pretty}"
    );
}
