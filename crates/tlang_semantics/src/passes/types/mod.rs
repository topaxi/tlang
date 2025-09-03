use std::cell::RefCell;
use std::rc::Rc;

use tlang_ast::Visitor;
use tlang_span::NodeId;
use tlang_symbols::SymbolTable;

use crate::{SemanticAnalysisGroup, SemanticAnalysisPass};

pub struct TypePass(SemanticAnalysisGroup);

impl TypePass {
    pub fn new() -> Self {
        Self(SemanticAnalysisGroup::new(
            std::any::type_name::<Self>(),
            vec![Box::new(TypeResolutionPass::default())],
        ))
    }
}

impl Default for TypePass {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticAnalysisPass for TypePass {
    fn init_context(&mut self, ctx: &mut crate::SemanticAnalysisContext) {
        self.0.init_context(ctx);
    }

    fn analyze(
        &mut self,
        module: &tlang_ast::node::Module,
        ctx: &mut crate::SemanticAnalysisContext,
        is_root: bool,
    ) {
        self.0.analyze(module, ctx, is_root);
    }
}

#[derive(Default)]
pub struct TypeResolutionPass {
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
}

impl TypeResolutionPass {
    fn current_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().cloned().unwrap()
    }

    fn push_symbol_table(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
        self.symbol_table_stack.push(symbol_table.clone());
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }
}

impl SemanticAnalysisPass for TypeResolutionPass {
    fn init_context(&mut self, ctx: &mut crate::SemanticAnalysisContext) {
        self.symbol_table_stack.clear();
        self.symbol_table_stack.push(ctx.root_symbol_table.clone());
    }

    fn analyze(
        &mut self,
        module: &tlang_ast::node::Module,
        ctx: &mut crate::SemanticAnalysisContext,
        _is_root: bool,
    ) {
        self.visit_module(module, ctx);

        // After collecting all declarations, we should be left with the root symbol table on the
        // symbol table stack.
        debug_assert_eq!(self.symbol_table_stack.len(), 1);
    }
}

impl<'ast> Visitor<'ast> for TypeResolutionPass {
    type Context = crate::SemanticAnalysisContext;

    fn enter_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        if let Some(symbol_table) = ctx.get_symbol_table(node_id) {
            self.push_symbol_table(&symbol_table);
        }
    }

    fn leave_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        if ctx.get_symbol_table(node_id).is_some() {
            self.pop_symbol_table();
        }
    }

    fn visit_ty(&mut self, ty: &'ast tlang_ast::node::Ty, ctx: &mut Self::Context) {
        let type_string = ty.name.to_string();
    }
}
