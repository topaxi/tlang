use tlang_ast::{node::Node, symbols::SymbolType};

use crate::declarations::DeclarationAnalyzer;

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new(DeclarationAnalyzer::default())
    }
}

impl SemanticAnalyzer {
    pub fn new(declaration_analyzer: DeclarationAnalyzer) -> Self {
        SemanticAnalyzer {
            declaration_analyzer,
        }
    }

    pub fn add_builtin_symbols(&mut self, symbols: Vec<(&str, SymbolType)>) {
        self.declaration_analyzer.add_builtin_symbols(symbols)
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        self.collect_declarations(ast);
        // self.collect_initializations(ast);
        // self.analyze_node(ast);
    }

    fn collect_declarations(&mut self, ast: &mut Node) {
        self.declaration_analyzer.analyze(ast);
    }
}
