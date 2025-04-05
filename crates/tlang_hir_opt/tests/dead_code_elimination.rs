use std::collections::HashSet;
use tlang_hir::{
    hir::{Block, Expr, ExprKind, HirId, Module, PatKind, StmtKind},
    visit::Visitor,
};
use tlang_hir_opt::{
    dead_code_elimination::DeadCodeEliminator,
    HirPass,
};

mod common;

#[derive(Default)]
struct PathCollector {
    paths: HashSet<(HirId, Option<usize>)>,
}

impl PathCollector {
    fn new() -> Self {
        Self {
            paths: HashSet::new(),
        }
    }

    fn collect(module: &mut Module) -> HashSet<(HirId, Option<usize>)> {
        let mut collector = Self::new();
        collector.visit_module(module);
        collector.paths
    }
}

impl<'hir> Visitor<'hir> for PathCollector {
    fn visit_block(&mut self, block: &'hir mut Block) {
        // First collect let bindings
        for stmt in &mut block.stmts {
            if let StmtKind::Let(pat, _, _) = &mut stmt.kind {
                if let PatKind::Identifier(hir_id, ref ident) = pat.kind {
                    println!("Found let binding: {} with hir_id: {:?}", ident.as_str(), hir_id);
                    self.paths.insert((hir_id, None));
                }
            }
        }

        // Then visit all expressions
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(hir_id) = path.res.hir_id() {
                println!("Found path usage: {} with hir_id: {:?} and res: {:?}", path.segments[0].ident.as_str(), hir_id, path.res);
                self.paths.insert((hir_id, path.res.slot_index()));
            }
        }

        match &mut expr.kind {
            ExprKind::Block(block) => {
                self.visit_block(block);
            }
            ExprKind::Call(call) => {
                self.visit_expr(&mut call.callee);
                for arg in &mut call.arguments {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Binary(_, lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::IfElse(cond, then_block, else_clauses) => {
                self.visit_expr(cond);
                self.visit_block(then_block);
                for else_clause in else_clauses {
                    if let Some(cond) = &mut else_clause.condition {
                        self.visit_expr(cond);
                    }
                    self.visit_block(&mut else_clause.consequence);
                }
            }
            _ => {}
        }
    }
}

#[test]
fn test_simple_dead_code_removal() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        y;
    "#;

    let mut module = common::compile(source);

    // Collect initial paths
    let initial_paths = PathCollector::collect(&mut module);
    println!("\nInitial paths:");
    for (hir_id, slot) in &initial_paths {
        println!("  hir_id: {:?} and slot: {:?}", hir_id, slot);
    }

    // Run dead code elimination
    let mut optimizer = DeadCodeEliminator::new();
    optimizer.optimize_module(&mut module);

    // Collect final paths
    let final_paths = PathCollector::collect(&mut module);
    println!("\nFinal paths:");
    for (hir_id, slot) in &final_paths {
        println!("  hir_id: {:?} and slot: {:?}", hir_id, slot);
    }

    // Verify that only 'y' remains
    let y_exists = final_paths.iter().any(|(_, slot)| slot.is_some());
    assert!(y_exists, "Expected to find 'y' in final paths");
}

#[test]
fn test_simple_slot_reassignment() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        y;
    "#;

    let mut module = common::compile(source);

    // Collect initial paths
    let initial_paths = PathCollector::collect(&mut module);
    let _initial_y = initial_paths.iter()
        .find(|(_, slot)| slot.is_some())
        .expect("Should find y in initial paths");

    // Run dead code elimination
    let mut optimizer = DeadCodeEliminator::new();
    optimizer.optimize_module(&mut module);

    // Collect final paths
    let final_paths = PathCollector::collect(&mut module);
    let final_y = final_paths.iter()
        .find(|(_, slot)| slot.is_some())
        .expect("Should find y in final paths");

    // Verify that y's slot index was updated
    assert_eq!(final_y.1.unwrap(), 0, "y's slot index should be updated to 0");
}

#[test]
fn test_nested_scope_slot_reassignment() {
    let source = r#"
        let x = 1;
        let y = 2;
        {
            let a = 3;
            let b = 4;
            b;
        };
    "#;

    let mut module = common::compile(source);

    // Collect initial paths
    let initial_paths = PathCollector::collect(&mut module);
    let _initial_b = initial_paths.iter()
        .find(|(_, slot)| slot.is_some())
        .expect("Should find b in initial paths");

    // Run dead code elimination
    let mut optimizer = DeadCodeEliminator::new();
    optimizer.optimize_module(&mut module);

    // Collect final paths
    let final_paths = PathCollector::collect(&mut module);
    let final_b = final_paths.iter()
        .find(|(_, slot)| slot.is_some())
        .expect("Should find b in final paths");

    // Verify that b's slot index was updated
    assert_eq!(final_b.1.unwrap(), 0, "b's slot index should be updated to 0");
} 