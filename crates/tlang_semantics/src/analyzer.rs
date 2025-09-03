use std::{cell::RefCell, collections::HashMap, rc::Rc};

use tlang_ast::{
    node::{Module, StructDeclaration},
    token::Literal,
    visit::Visitor,
};
use tlang_span::{NodeId, Span};
use tlang_symbols::{SymbolIdAllocator, SymbolTable, SymbolType};

use crate::{
    declarations::{DeclarationAnalyzer, DeclarationContext},
    diagnostic::{self, Diagnostic},
    variable_usage::VariableUsageValidator,
};

/**
 * Context for semantic analysis, containing only shared state needed
 * during the visitor traversal (similar to DeclarationContext).
 */
pub struct SemanticAnalysisContext {
    pub declaration_context: DeclarationContext,
    pub struct_declarations: HashMap<String, StructDeclaration>,
}

impl SemanticAnalysisContext {
    pub fn new(declaration_context: DeclarationContext) -> Self {
        SemanticAnalysisContext {
            declaration_context,
            struct_declarations: HashMap::new(),
        }
    }

    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.declaration_context.symbol_tables().get(&id).cloned()
    }
}

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    variable_usage_validator: VariableUsageValidator,
    context: Option<SemanticAnalysisContext>,
    diagnostics: Vec<Diagnostic>,
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
            variable_usage_validator: VariableUsageValidator::new(),
            context: None,
            diagnostics: vec![],
        }
    }

    #[inline(always)]
    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.symbol_tables()
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables().get(&id).cloned()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn root_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.root_symbol_table().clone()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.symbol_id_allocator()
    }

    pub fn get_diagnostics(&self) -> Vec<Diagnostic> {
        let mut all_diagnostics = self.diagnostics.clone();
        all_diagnostics.extend(
            self.variable_usage_validator
                .get_diagnostics()
                .iter()
                .cloned(),
        );
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
            let declaration_context = DeclarationContext::new();
            self.context = Some(SemanticAnalysisContext::new(declaration_context));
        }

        if let Some(ref mut ctx) = self.context {
            ctx.declaration_context.add_builtin_symbols(symbols);
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
        // First run declaration analysis to collect all declarations
        let declaration_context = if let Some(ref mut existing_ctx) = self.context {
            // Reuse existing context with builtin symbols
            self.declaration_analyzer.analyze_with_context(
                module,
                is_root,
                &mut existing_ctx.declaration_context,
            );
            std::mem::take(&mut existing_ctx.declaration_context)
        } else {
            // Create new context
            self.declaration_analyzer.analyze(module, is_root)
        };

        // Create analysis context and reset analyzer state
        let mut context = SemanticAnalysisContext::new(declaration_context);
        self.diagnostics.clear();
        self.variable_usage_validator = VariableUsageValidator::new();

        // Run semantic analysis for struct declarations and other non-variable-usage concerns
        self.visit_module(module, &mut context);

        // Run variable usage validation as a separate pass
        let symbol_tables = context.declaration_context.symbol_tables().clone();
        let root_table = context.declaration_context.root_symbol_table().clone();
        self.variable_usage_validator.validate_module(module, symbol_tables, root_table);

        // Store context
        self.context = Some(context);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors())
        }
    }

    fn validate_escape_sequences(&mut self, string_content: &str, span: Span) {
        let mut chars = string_content.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\'
                && let Some(&next_ch) = chars.peek()
            {
                // Check if this is a valid escape sequence
                // Valid escape sequences in tlang: \", \', \\, \n, \t, \r, \0, \u{...}
                match next_ch {
                    '"' | '\'' | '\\' | 'n' | 't' | 'r' | '0' => {
                        // Valid escape sequence, no warning needed
                        chars.next(); // consume the character after backslash
                    }
                    'u' => {
                        // Check for Unicode escape sequence \u{...}
                        chars.next(); // consume 'u'
                        if let Some(&'{') = chars.peek() {
                            chars.next(); // consume '{'
                            let mut hex_digits = String::new();
                            let mut found_closing_brace = false;

                            // Collect hex digits until we find '}'
                            while let Some(&hex_ch) = chars.peek() {
                                if hex_ch == '}' {
                                    chars.next(); // consume '}'
                                    found_closing_brace = true;
                                    break;
                                } else if hex_ch.is_ascii_hexdigit() && hex_digits.len() < 6 {
                                    hex_digits.push(hex_ch);
                                    chars.next();
                                } else {
                                    // Invalid character or too many digits
                                    break;
                                }
                            }

                            // Validate the Unicode escape sequence
                            if !found_closing_brace {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Unterminated Unicode escape sequence in string literal",
                                ));
                            } else if hex_digits.is_empty() {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Empty Unicode escape sequence in string literal",
                                ));
                            } else if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                if char::from_u32(code_point).is_none() {
                                    self.diagnostics.push(diagnostic::warn_at!(
                                        span,
                                        "Invalid Unicode code point in string literal",
                                    ));
                                }
                                // Valid Unicode escape sequence, no warning needed
                            } else {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Invalid hexadecimal in Unicode escape sequence",
                                ));
                            }
                        } else {
                            // \u not followed by {, treat as unknown escape sequence
                            self.diagnostics.push(diagnostic::warn_at!(
                                span,
                                "Unknown escape sequence '\\{}' in string literal",
                                next_ch
                            ));
                        }
                    }
                    _ => {
                        // Unknown escape sequence, emit warning
                        self.diagnostics.push(diagnostic::warn_at!(
                            span,
                            "Unknown escape sequence '\\{}' in string literal",
                            next_ch
                        ));
                        chars.next(); // consume the character after backslash
                    }
                }
            }
        }
    }
}

impl<'ast> Visitor<'ast> for SemanticAnalyzer {
    type Context = SemanticAnalysisContext;

    fn visit_struct_decl(
        &mut self,
        decl: &'ast tlang_ast::node::StructDeclaration,
        ctx: &mut Self::Context,
    ) {
        ctx.struct_declarations
            .insert(decl.name.to_string(), decl.clone());
    }

    fn visit_literal(&mut self, literal: &'ast Literal, span: Span, _ctx: &mut Self::Context) {
        match literal {
            Literal::String(string_content) | Literal::Char(string_content) => {
                self.validate_escape_sequences(string_content, span);
            }
            _ => {
                // No validation needed for other literal types
            }
        }
    }
}
