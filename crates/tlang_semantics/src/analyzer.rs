use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;
use tlang_ast::node::{Module, StructDeclaration};
use tlang_span::NodeId;
use tlang_symbols::{SymbolIdAllocator, SymbolTable, SymbolType};

use crate::{
    declarations::DeclarationAnalyzer, diagnostic::Diagnostic,
    misc_analysis::MiscellaneousAnalyzer, variable_usage::VariableUsageValidator,
};

/**
 * Context for semantic analysis, containing shared state needed
 * across all semantic analysis passes.
 */
pub struct SemanticAnalysisContext {
    pub symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    pub symbol_id_allocator: SymbolIdAllocator,
    pub root_symbol_table: Rc<RefCell<SymbolTable>>,
    pub struct_declarations: HashMap<String, StructDeclaration>,
    pub diagnostics: Vec<Diagnostic>,
}

impl SemanticAnalysisContext {
    pub fn new() -> Self {
        let root_symbol_table = Rc::new(RefCell::new(SymbolTable::default()));
        SemanticAnalysisContext {
            symbol_tables: HashMap::from([(NodeId::new(1), root_symbol_table.clone())]),
            symbol_id_allocator: SymbolIdAllocator::default(),
            root_symbol_table,
            struct_declarations: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables.get(&id).cloned()
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn get_diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        use tlang_symbols::SymbolInfo;

        for (name, symbol_type) in symbols {
            let symbol_info = SymbolInfo::new_builtin(
                self.symbol_id_allocator.next_id(),
                name.as_ref(),
                *symbol_type,
            );
            self.root_symbol_table.borrow_mut().insert(symbol_info);
        }
    }
}

impl Default for SemanticAnalysisContext {
    fn default() -> Self {
        Self::new()
    }
}

/**
 * Trait for semantic analysis passes, similar to HirPass in the HIR optimizer.
 */
pub trait SemanticAnalysisPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    #[allow(unused_variables)]
    fn init_context(&mut self, ctx: &mut SemanticAnalysisContext) {}

    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, is_root: bool);
}

/**
 * Group of semantic analysis passes that implements SemanticAnalysisPass.
 * Similar to HirOptGroup in the HIR optimizer.
 */
#[derive(Default)]
pub struct SemanticAnalysisGroup {
    name: &'static str,
    passes: Vec<Box<dyn SemanticAnalysisPass>>,
}

impl SemanticAnalysisGroup {
    pub fn new(name: &'static str, passes: Vec<Box<dyn SemanticAnalysisPass>>) -> Self {
        Self { name, passes }
    }

    pub fn add_pass(&mut self, pass: Box<dyn SemanticAnalysisPass>) {
        self.passes.push(pass);
    }
}

impl SemanticAnalysisPass for SemanticAnalysisGroup {
    fn name(&self) -> &'static str {
        self.name
    }

    fn init_context(&mut self, ctx: &mut SemanticAnalysisContext) {
        for pass in &mut self.passes {
            debug!("Initializing context for pass: {}", pass.name());
            pass.init_context(ctx);
        }
    }

    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, is_root: bool) {
        for pass in &mut self.passes {
            debug!("Running semantic analysis pass: {}", pass.name());
            pass.analyze(module, ctx, is_root);
        }
    }
}

pub struct SemanticAnalyzer {
    group: SemanticAnalysisGroup,
    context: Option<SemanticAnalysisContext>,
    diagnostics: Vec<Diagnostic>,
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new(vec![
            Box::new(DeclarationAnalyzer::default()),
            Box::new(VariableUsageValidator::default()),
            Box::new(MiscellaneousAnalyzer),
        ])
    }
}

impl SemanticAnalyzer {
    pub fn new(passes: Vec<Box<dyn SemanticAnalysisPass>>) -> Self {
        SemanticAnalyzer {
            group: SemanticAnalysisGroup::new("semantic_analysis", passes),
            context: None,
            diagnostics: vec![],
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn SemanticAnalysisPass>) {
        self.group.add_pass(pass);
    }

    #[inline(always)]
    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        &ctx.symbol_tables
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables().get(&id).cloned()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn root_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.root_symbol_table.clone()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.symbol_id_allocator
    }

    pub fn get_diagnostics(&self) -> Vec<Diagnostic> {
        let mut all_diagnostics = self.diagnostics.clone();

        // Include diagnostics from the context
        if let Some(ref ctx) = self.context {
            all_diagnostics.extend(ctx.get_diagnostics().iter().cloned());
        }

        all_diagnostics
    }

    pub fn get_errors(&self) -> Vec<Diagnostic> {
        self.get_diagnostics()
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .cloned()
            .collect()
    }

    pub fn get_struct_declaration(&self, name: &str) -> Option<&StructDeclaration> {
        self.context.as_ref()?.struct_declarations.get(name)
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        // Initialize context if it doesn't exist
        if self.context.is_none() {
            self.context = Some(SemanticAnalysisContext::new());
        }

        if let Some(ref mut ctx) = self.context {
            ctx.add_builtin_symbols(symbols);
        }
    }

    pub fn analyze(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, false)
    }

    pub fn analyze_as_root_module(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, true)
    }

    fn analyze_root_module(
        &mut self,
        module: &Module,
        is_root: bool,
    ) -> Result<(), Vec<Diagnostic>> {
        // Initialize or reuse existing context
        let mut context = self.context.take().unwrap_or_default();

        // Reset analyzer state
        self.diagnostics.clear();
        context.diagnostics.clear();

        // Initialize context for all passes
        self.group.init_context(&mut context);

        // Run all semantic analysis passes
        self.group.analyze(module, &mut context, is_root);

        // Store context
        self.context = Some(context);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors())
        }
    }
}
