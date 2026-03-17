#![cfg(feature = "binary")]
#![allow(dead_code)]

use std::collections::HashSet;

use tlang_ast_lowering::lower_to_hir;
use tlang_hir as hir;
use tlang_hir::HirId;
use tlang_interpreter::{EvalResult, Interpreter};
use tlang_memory::{TlangValue, VMState};
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_vm::VM;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

fn make_state() -> VMState {
    VM::new().into_state()
}

fn make_analyzer() -> SemanticAnalyzer {
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols_with_slots(&VM::builtin_symbols());
    analyzer
}

fn compile(source: &str, analyzer: &mut SemanticAnalyzer) -> (hir::Module, HashSet<HirId>) {
    // Wrap in a block expression so the completion value is accessible.
    let wrapped = format!("{{ {source} }};");
    let mut parser = Parser::from_source(&wrapped);
    let mut ast = parser.parse().expect("parse error");
    analyzer.analyze(&mut ast).expect("semantic error");

    let (mut module, meta) = lower_to_hir(
        &ast,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    );

    let mut optimizer = tlang_hir_opt::HirOptimizer::default();
    let constant_pool_ids = meta.constant_pool_ids.clone();
    let mut ctx = meta.into();
    optimizer.optimize_hir(&mut module, &mut ctx);
    (module, constant_pool_ids)
}

fn eval_block_expr(state: &mut VMState, module: &hir::Module) -> TlangValue {
    let hir::StmtKind::Expr(expr) = &module.block.stmts[0].kind else {
        panic!(
            "Expected Expr statement, got {:?}",
            &module.block.stmts[0].kind
        );
    };
    match Interpreter.eval_expr(state, expr) {
        EvalResult::Value(v) | EvalResult::Return(v) | EvalResult::Break(v) => v,
        EvalResult::Void => TlangValue::Nil,
        other => panic!("unexpected eval result: {other:?}"),
    }
}

/// Evaluate a tlang source snippet and return the final value.
pub fn eval(source: &str) -> TlangValue {
    let mut state = make_state();
    let mut analyzer = make_analyzer();
    let (module, constant_pool_ids) = compile(source, &mut analyzer);
    state.register_constant_pool_ids(constant_pool_ids);
    eval_block_expr(&mut state, &module)
}

/// Evaluate source and return the stringified result.
pub fn eval_to_string(source: &str) -> String {
    let mut state = make_state();
    let mut analyzer = make_analyzer();
    let (module, constant_pool_ids) = compile(source, &mut analyzer);
    state.register_constant_pool_ids(constant_pool_ids);
    let value = eval_block_expr(&mut state, &module);
    state.stringify(value)
}
