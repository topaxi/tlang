use crate::diagnostic::{self, Diagnostic};
use std::{cell::RefCell, rc::Rc};
use tlang_span::Span;
use tlang_symbols::{SymbolInfo, SymbolTable};

/// Pass for validating variable usage, handling both unused variables
/// and undeclared variable references.
pub struct VariableUsageValidator {
    diagnostics: Vec<Diagnostic>,
}

impl VariableUsageValidator {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// Get all diagnostics collected during validation
    pub fn get_diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Take all diagnostics, clearing the internal list
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Report unused symbols in the given symbol table
    pub fn report_unused_symbols(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
        let symbol_table = symbol_table.borrow();
        let unused_symbols = symbol_table
            .get_all_declared_local_symbols()
            .filter(|symbol| !symbol.used)
            .filter(|symbol| !symbol.is_builtin())
            // Do not report the binding introduced within function bodies to reference to
            // themselves.
            .filter(|symbol| !symbol.is_fn_self_binding())
            .filter(|symbol| !symbol.name.starts_with('_'))
            // TODO: We currently do not track member methods, as we do not have any type
            //       information yet.
            .filter(|symbol| !symbol.name.contains('.'))
            .collect::<Vec<_>>();

        for unused_symbol in &unused_symbols {
            if unused_symbol.is_any_fn() {
                self.diagnostics.push(diagnostic::warn_at!(
                    unused_symbol.defined_at,
                    "Unused {} `{}/{}`",
                    unused_symbol.symbol_type,
                    unused_symbol.name,
                    unused_symbol.symbol_type.arity().unwrap(),
                ));
            } else {
                self.diagnostics.push(diagnostic::warn_at!(
                    unused_symbol.defined_at,
                    "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                    unused_symbol.symbol_type, unused_symbol.name, unused_symbol.name
                ));
            }
        }
    }

    /// Report an undeclared variable reference
    pub fn report_undeclared_variable(
        &mut self,
        name: &str,
        span: Span,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) {
        let did_you_mean = did_you_mean(name, &symbol_table.borrow().get_all_declared_symbols());

        if let Some(suggestion) = did_you_mean {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared variable `{name}`, did you mean the {} `{}`?",
                suggestion.symbol_type,
                suggestion.name,
            ));
        } else {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared variable `{name}`",
            ));
        }
    }

    /// Report an undeclared function reference
    pub fn report_undeclared_function(
        &mut self,
        name: &str,
        arity: usize,
        span: Span,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) {
        let did_you_mean = did_you_mean(name, &symbol_table.borrow().get_all_declared_symbols());

        if let Some(suggestion) = did_you_mean {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared function `{name}` with arity {arity}, did you mean the {} `{}`?",
                suggestion.symbol_type,
                suggestion.name
            ));
        } else {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared function `{name}` with arity {arity}",
            ));
        }
    }
}

impl Default for VariableUsageValidator {
    fn default() -> Self {
        Self::new()
    }
}

fn did_you_mean(name: &str, candidates: &[SymbolInfo]) -> Option<SymbolInfo> {
    let mut best_distance = usize::MAX;
    let mut best_candidate = None;
    for candidate in candidates {
        let distance = levenshtein_distance(name, &candidate.name);
        if distance < best_distance {
            best_distance = distance;
            best_candidate = Some(candidate);
        }
    }
    if best_distance < 3 {
        Some(best_candidate.unwrap().clone())
    } else {
        None
    }
}

fn levenshtein_distance(a: &str, b: &str) -> usize {
    let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];
    #[allow(clippy::needless_range_loop)]
    for i in 0..=a.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b.len() {
        matrix[0][j] = j;
    }
    for j in 1..=b.len() {
        for i in 1..=a.len() {
            let substitution_cost = if a.chars().nth(i - 1) == b.chars().nth(j - 1) {
                0
            } else {
                1
            };
            matrix[i][j] = [
                matrix[i - 1][j] + 1,
                matrix[i][j - 1] + 1,
                matrix[i - 1][j - 1] + substitution_cost,
            ]
            .into_iter()
            .min()
            .unwrap();
        }
    }
    matrix[a.len()][b.len()]
}
