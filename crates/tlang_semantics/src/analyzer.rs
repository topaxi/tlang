use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use log::debug;
use tlang_ast::node::{Module, StructDeclaration};
use tlang_defs::{DefIdAllocator, DefKind, DefScope};
use tlang_span::{NodeId, Span};

use crate::{
    diagnostic::Diagnostic,
    passes::{
        DeclarationAnalyzer, FnParamTypeInference, ProtocolConstraintValidator,
        StringLiteralValidator, VariableUsageValidator, VisibilityValidator,
    },
};

/// Context for semantic analysis, containing shared state needed
/// across all semantic analysis passes.
pub struct SemanticAnalysisContext {
    pub symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>>,
    pub symbol_id_allocator: DefIdAllocator,
    pub root_symbol_table: Arc<RwLock<DefScope>>,
    pub struct_declarations: HashMap<String, StructDeclaration>,
    pub diagnostics: Vec<Diagnostic>,
    /// Maps protocol name → list of direct constraint protocol names.
    pub protocol_constraints: HashMap<String, Vec<String>>,
    /// Tracks which (protocol, type) pairs have impl blocks, along with the
    /// span of the impl block for use in diagnostics and whether it's a blanket impl.
    pub protocol_impls: Vec<(String, String, Span, bool)>,
    /// Maps protocol name → list of associated type names.
    pub protocol_associated_types: HashMap<String, Vec<String>>,
    /// Tracks associated type bindings per impl: (protocol, type) → list of bound names.
    pub impl_associated_type_bindings: Vec<(String, String, Vec<String>, Span)>,
    /// Where clause predicates per impl: (protocol, type, predicates, span).
    pub impl_where_predicates: Vec<(String, String, Vec<(String, Vec<String>)>, Span)>,
}

impl SemanticAnalysisContext {
    pub fn new() -> Self {
        let root_symbol_table = Arc::new(RwLock::new(DefScope::default()));
        SemanticAnalysisContext {
            symbol_tables: HashMap::from([(NodeId::new(1), root_symbol_table.clone())]),
            symbol_id_allocator: DefIdAllocator::default(),
            root_symbol_table,
            struct_declarations: HashMap::new(),
            diagnostics: Vec::new(),
            protocol_constraints: HashMap::new(),
            protocol_impls: Vec::new(),
            protocol_associated_types: HashMap::new(),
            impl_associated_type_bindings: Vec::new(),
            impl_where_predicates: Vec::new(),
        }
    }

    pub fn get_symbol_table(&self, id: NodeId) -> Option<Arc<RwLock<DefScope>>> {
        self.symbol_tables.get(&id).cloned()
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn get_diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// # Panics
    ///
    /// Panics if the internal symbol table lock is poisoned.
    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, DefKind)>,
    {
        use tlang_defs::Def;

        for (name, kind) in symbols {
            let symbol_info = Def::new_builtin(
                self.symbol_id_allocator.next_id(),
                name.as_ref(),
                *kind,
                None,
            );
            self.root_symbol_table.write().unwrap().insert(symbol_info);
        }
    }

    /// # Panics
    ///
    /// Panics if the internal symbol table lock is poisoned.
    pub fn add_builtin_symbols_with_slots<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, DefKind, Option<usize>)>,
    {
        use tlang_defs::Def;

        for (name, kind, global_slot) in symbols {
            let symbol_info = Def::new_builtin(
                self.symbol_id_allocator.next_id(),
                name.as_ref(),
                *kind,
                *global_slot,
            );
            self.root_symbol_table.write().unwrap().insert(symbol_info);
        }
    }
}

impl Default for SemanticAnalysisContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for semantic analysis passes, similar to `HirPass` in the HIR optimizer.
pub trait SemanticAnalysisPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    #[allow(unused_variables)]
    fn init_context(&mut self, ctx: &mut SemanticAnalysisContext) {}

    /// Mutate the AST before symbol-table analysis runs. The default is a no-op.
    #[allow(unused_variables)]
    fn mutate(&mut self, module: &mut Module) {}

    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, is_root: bool);
}

/// Group of semantic analysis passes that implements `SemanticAnalysisPass`.
/// Similar to `HirOptGroup` in the HIR optimizer.
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

    fn mutate(&mut self, module: &mut Module) {
        for pass in &mut self.passes {
            pass.mutate(module);
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
            Box::new(FnParamTypeInference::default()),
            Box::new(DeclarationAnalyzer::default()),
            Box::new(ProtocolConstraintValidator),
            Box::new(VariableUsageValidator::default()),
            Box::new(StringLiteralValidator),
            Box::new(VisibilityValidator),
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
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Arc<RwLock<DefScope>>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        &ctx.symbol_tables
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Arc<RwLock<DefScope>>> {
        self.symbol_tables().get(&id).cloned()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn root_symbol_table(&self) -> Arc<RwLock<DefScope>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.root_symbol_table.clone()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_id_allocator(&self) -> DefIdAllocator {
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
        I: IntoIterator<Item = &'a (S, DefKind)>,
    {
        // Initialize context if it doesn't exist
        if self.context.is_none() {
            self.context = Some(SemanticAnalysisContext::new());
        }

        if let Some(ref mut ctx) = self.context {
            ctx.add_builtin_symbols(symbols);
        }
    }

    pub fn add_builtin_symbols_with_slots<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, DefKind, Option<usize>)>,
    {
        // Initialize context if it doesn't exist
        if self.context.is_none() {
            self.context = Some(SemanticAnalysisContext::new());
        }

        if let Some(ref mut ctx) = self.context {
            ctx.add_builtin_symbols_with_slots(symbols);
        }
    }

    /// Returns true if a symbol with the given name is already registered as a builtin.
    ///
    /// # Panics
    ///
    /// Panics if the internal symbol table lock is poisoned.
    pub fn has_builtin_symbol(&self, name: &str) -> bool {
        self.context
            .as_ref()
            .map(|ctx| ctx.root_symbol_table.read().unwrap().has_name(name))
            .unwrap_or(false)
    }

    pub fn analyze(&mut self, module: &mut Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, false)
    }

    pub fn analyze_as_root_module(&mut self, module: &mut Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, true)
    }

    fn analyze_root_module(
        &mut self,
        module: &mut Module,
        is_root: bool,
    ) -> Result<(), Vec<Diagnostic>> {
        // Initialize or reuse existing context
        let mut context = self.context.take().unwrap_or_default();

        // Reset analyzer state
        self.diagnostics.clear();
        context.diagnostics.clear();

        // Initialize context for all passes
        self.group.init_context(&mut context);

        // Run mutation passes first (e.g. type annotation inference)
        self.group.mutate(module);

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
