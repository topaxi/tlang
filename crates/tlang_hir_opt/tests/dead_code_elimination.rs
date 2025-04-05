use tlang_hir::{
    hir::{Expr, ExprKind, HirId, Module, Path},
    visit::{Visitor, walk_expr},
};
use tlang_hir_opt::DeadCodeEliminator;

mod common;

struct PathCollector {
    paths: Vec<(HirId, Path)>,
}

impl PathCollector {
    fn new() -> Self {
        Self { paths: Vec::new() }
    }

    fn collect(module: &mut Module) -> Vec<(HirId, Path)> {
        let mut collector = Self::new();
        collector.visit_module(module);
        collector.paths
    }
}

impl<'hir> Visitor<'hir> for PathCollector {
    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(hir_id) = path.res.hir_id() {
                self.paths.push((hir_id, (**path).clone()));
            }
        }

        walk_expr(self, expr);
    }
}

#[test]
fn test_simple_slot_reassignment() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        println(y);  // Only y is used, x and z should be eliminated
    "#;

    // Collect initial paths and their slots
    let mut hir = common::compile_to_hir(source);
    let initial_paths = PathCollector::collect(&mut hir.clone());

    // Run optimizer
    common::optimize(&mut hir, vec![
        Box::new(DeadCodeEliminator::default()),
    ]);

    // Collect paths after optimization
    let final_paths = PathCollector::collect(&mut hir.clone());

    // Verify that:
    // 1. We have fewer paths after optimization (x and z references removed)
    // 2. The remaining path (y) has its slot index updated
    assert!(final_paths.len() < initial_paths.len(), "Expected fewer paths after optimization");
    
    // Find the path for 'y' before and after
    let initial_y = initial_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "y")
        .expect("Should find y in initial paths");
    let final_y = final_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "y")
        .expect("Should find y in final paths");

    // The slot for y should be 0 after optimization since x was removed
    assert_ne!(
        initial_y.1.res.slot_index(),
        final_y.1.res.slot_index(),
        "y's slot should have changed"
    );
    assert_eq!(
        final_y.1.res.slot_index().unwrap(),
        0,
        "y should now be in slot 0"
    );
}

#[test]
fn test_nested_scope_slot_reassignment() {
    let source = r#"
        let x = 1;
        let y = 2;
        if true {
            let a = 3;
            let b = 4;
            println(y, b);
        }
    "#;

    let mut hir = common::compile_to_hir(source);
    let initial_paths = PathCollector::collect(&mut hir.clone());

    common::optimize(&mut hir, vec![
        Box::new(DeadCodeEliminator::default()),
    ]);

    let final_paths = PathCollector::collect(&mut hir.clone());

    // Verify paths were removed and slots were updated
    assert!(final_paths.len() < initial_paths.len(), "Expected fewer paths after optimization");

    // Find paths for 'y' and 'b' before and after
    let initial_y = initial_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "y")
        .expect("Should find y in initial paths");
    let final_y = final_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "y")
        .expect("Should find y in final paths");

    let initial_b = initial_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "b")
        .expect("Should find b in initial paths");
    let final_b = final_paths.iter()
        .find(|(_, p)| p.segments[0].ident.to_string() == "b")
        .expect("Should find b in final paths");

    // y should be in slot 0 of outer scope
    assert_eq!(
        final_y.1.res.slot_index().unwrap(),
        0,
        "y should be in slot 0"
    );

    // b should be in slot 0 of inner scope
    assert_eq!(
        final_b.1.res.slot_index().unwrap(),
        0,
        "b should be in slot 0 of its scope"
    );
} 