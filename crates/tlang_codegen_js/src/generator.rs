use crate::{pattern_match_generator::MatchContextStack, scope::Scope};
use tlang_ast::{
    symbols::SymbolType,
    token::{Token, TokenKind},
};
use tlang_hir::hir;

// Before we indent a line, we reserve at least the indentation space plus some more for the the
// next statement. We start with an assumption of 128 below, this might the maximum overhead once
// we are done generating the code.
const STATEMENT_RESERVE_SIZE: usize = 128;

// When we create a new statement buffer, we reserve a certain amount of space for the statements.
const STATEMENT_BUFFER_CAPACITY: usize = 512;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BlockContext {
    // Are we in a top level Program?
    Program,
    // Are we in a StatementExpression with a Block?
    Statement,
    // Are we in an expression with a Block?
    Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionContext {
    // The name of the current function we're in.
    // Empty string for anonymous functions.
    pub name: String,

    // The parameter name bindings of the current function we're in.
    pub parameter_bindings: Vec<String>,

    // Is the current function body tail recursive?
    // This is used to determine if we should unwrap the recursion into a while loop.
    pub is_tail_recursive: bool,
}

#[derive(Debug)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    scopes: Scope,
    context_stack: Vec<BlockContext>,
    function_context_stack: Vec<FunctionContext>,
    pub(crate) match_context_stack: MatchContextStack,
    statement_buffer: Vec<String>,
    completion_variables: Vec<Option<Box<str>>>,
    render_ternary: bool,
}

impl Default for CodegenJS {
    fn default() -> Self {
        Self::new()
    }
}

impl CodegenJS {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            scopes: Scope::default(),
            context_stack: vec![BlockContext::Program],
            function_context_stack: vec![],
            match_context_stack: Default::default(),
            statement_buffer: vec![String::with_capacity(STATEMENT_BUFFER_CAPACITY)],
            completion_variables: vec![None],
            render_ternary: true,
        }
    }

    pub fn set_render_ternary(&mut self, render_ternary: bool) {
        self.render_ternary = render_ternary;
    }

    pub(crate) fn get_render_ternary(&self) -> bool {
        self.render_ternary
    }

    pub fn get_standard_library_source() -> String {
        let stdlib_js_src = include_str!("../std/lib.tlang");
        let notice = r#"// This file was auto-generated by the tlang compiler."#;

        format!("{}\n{}", notice, stdlib_js_src)
    }

    pub fn get_standard_library_symbols() -> &'static [(&'static str, SymbolType); 29] {
        &[
            ("Option", SymbolType::Enum),
            ("Result", SymbolType::Enum),
            ("Option::Some", SymbolType::EnumVariant),
            ("Option::None", SymbolType::EnumVariant),
            ("Result::Ok", SymbolType::EnumVariant),
            ("Result::Err", SymbolType::EnumVariant),
            ("Some", SymbolType::Function),
            ("None", SymbolType::Variable),
            ("Ok", SymbolType::Function),
            ("Err", SymbolType::Function),
            ("len", SymbolType::Function),
            ("log", SymbolType::Function),
            ("math", SymbolType::Module),
            ("math::pi", SymbolType::Variable),
            ("math::max", SymbolType::Function),
            ("math::min", SymbolType::Function),
            ("math::floor", SymbolType::Function),
            ("math::random", SymbolType::Function),
            ("random_int", SymbolType::Function),
            ("math::sqrt", SymbolType::Function),
            ("compose", SymbolType::Function),
            ("map", SymbolType::Function),
            ("filter", SymbolType::Function),
            ("filter_map", SymbolType::Function),
            ("partition", SymbolType::Function),
            ("foldl", SymbolType::Function),
            ("foldr", SymbolType::Function),
            ("sum", SymbolType::Function),
            ("zip", SymbolType::Function),
        ]
    }

    pub(crate) fn push_statement_buffer(&mut self) {
        self.statement_buffer
            .push(String::with_capacity(STATEMENT_BUFFER_CAPACITY));
    }

    #[must_use]
    pub(crate) fn replace_statement_buffer(&mut self, buffer: String) -> String {
        let buf = self.statement_buffer.pop();
        self.statement_buffer.push(buffer);
        buf.unwrap()
    }

    #[must_use]
    pub(crate) fn replace_statement_buffer_with_empty_string(&mut self) -> String {
        self.replace_statement_buffer(String::with_capacity(STATEMENT_BUFFER_CAPACITY))
    }

    pub(crate) fn pop_statement_buffer(&mut self) -> String {
        debug_assert!(
            self.statement_buffer.len() != 1,
            "Cannot pop last statement buffer."
        );

        self.statement_buffer.pop().unwrap()
    }

    #[inline(always)]
    pub(crate) fn push_str(&mut self, str: &str) {
        self.statement_buffer.last_mut().unwrap().push_str(str);
    }

    #[inline(always)]
    pub(crate) fn push_char(&mut self, char: char) {
        self.statement_buffer.last_mut().unwrap().push(char);
    }

    #[inline(always)]
    pub(crate) fn push_newline(&mut self) {
        self.push_char('\n');
    }

    #[inline(always)]
    pub(crate) fn push_indent(&mut self) {
        // We are starting a new line, we should reserve at least the space for the indentation
        // plus some extra space for the statement.
        let reserve = self.indent_level * 4 + STATEMENT_RESERVE_SIZE;

        self.statement_buffer.last_mut().unwrap().reserve(reserve);

        for _ in 0..self.indent_level {
            self.push_str("    ");
        }
    }

    #[inline(always)]
    pub(crate) fn inc_indent(&mut self) {
        self.indent_level += 1;
    }

    #[inline(always)]
    pub(crate) fn dec_indent(&mut self) {
        self.indent_level -= 1;
    }

    #[inline(always)]
    pub(crate) fn current_indent(&self) -> usize {
        self.indent_level
    }

    #[inline(always)]
    pub(crate) fn set_indent(&mut self, indent: usize) {
        self.indent_level = indent;
    }

    #[inline(always)]
    pub(crate) fn flush_statement_buffer(&mut self) {
        self.output.push_str(self.statement_buffer.last().unwrap());
        self.statement_buffer.last_mut().unwrap().clear();
    }

    #[inline(always)]
    pub(crate) fn current_scope(&mut self) -> &mut Scope {
        &mut self.scopes
    }

    #[inline(always)]
    pub(crate) fn push_scope(&mut self) {
        self.scopes = Scope::new(Some(Box::new(self.scopes.clone())));
    }

    #[inline(always)]
    pub(crate) fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes.get_parent() {
            self.scopes = parent.clone();
        }
    }

    #[inline(always)]
    pub(crate) fn current_context(&self) -> BlockContext {
        *self.context_stack.last().unwrap()
    }

    #[inline(always)]
    pub(crate) fn push_context(&mut self, context: BlockContext) {
        self.context_stack.push(context);
    }

    #[inline(always)]
    pub(crate) fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    pub fn generate_code(&mut self, module: &hir::Module) {
        self.generate_statements(&module.block.stmts);
        self.output.shrink_to_fit();
    }

    pub(crate) fn get_function_context(&self) -> Option<&FunctionContext> {
        self.function_context_stack.last()
    }

    pub(crate) fn push_function_context(
        &mut self,
        name: &str,
        parameters: &[hir::FunctionParameter],
        parameter_bindings: &[String],
        is_tail_recursive: bool,
    ) {
        // We currently only need the parameter names/bindings for tail recursive functions.
        let parameter_bindings = if !is_tail_recursive {
            vec![]
        // If we have parameter_bindings provided, we use them to assign the parameters in
        // recursive function calls.
        } else if parameters.len() == parameter_bindings.len() {
            parameter_bindings.to_vec()
        } else {
            parameters
                .iter()
                .map(|param| {
                    if param.name.is_wildcard() {
                        // Encountered wildcard, declare tmp variable to potentially dereference
                        // from. Unused at the moment, as there's no way to reference a wildcard.
                        self.scopes.declare_tmp_variable()
                    } else {
                        param.name.to_string()
                    }
                })
                .collect()
        };

        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            parameter_bindings,
            is_tail_recursive,
        });
    }

    pub(crate) fn pop_function_context(&mut self) {
        self.function_context_stack.pop();
    }

    #[inline(always)]
    pub(crate) fn push_completion_variable(&mut self, name: Option<&str>) {
        self.completion_variables.push(name.map(|name| name.into()));
    }

    #[inline(always)]
    pub(crate) fn pop_completion_variable(&mut self) -> Option<Box<str>> {
        self.completion_variables.pop()?
    }

    #[inline(always)]
    pub(crate) fn current_completion_variable(&self) -> Option<&str> {
        self.completion_variables.last().unwrap().as_deref()
    }

    #[inline(always)]
    pub(crate) fn current_completion_variable_count(&self) -> usize {
        self.completion_variables.len()
    }

    #[inline(always)]
    pub(crate) fn nth_completion_variable(&self, index: usize) -> Option<&str> {
        self.completion_variables.get(index).unwrap().as_deref()
    }

    #[inline(always)]
    pub(crate) fn push_current_completion_variable(&mut self) {
        let current_completion_variable = self.pop_completion_variable();

        if let Some(str) = &current_completion_variable {
            self.push_str(str);
        }

        self.completion_variables.push(current_completion_variable);
    }

    pub(crate) fn push_let_declaration(&mut self, name: &str) {
        self.push_indent();
        self.push_str("let ");
        self.push_str(name);
        self.push_char(';');
    }

    pub(crate) fn push_open_let_declaration(&mut self, name: &str) {
        self.push_indent();
        self.push_str("let ");
        self.push_str(name);
        self.push_str(" = ");
    }

    pub(crate) fn push_let_declaration_to_expr(&mut self, name: &str, expr: &hir::Expr) {
        self.push_open_let_declaration(name);
        self.generate_expr(expr, None);
    }

    pub(crate) fn generate_comment(&mut self, comment: &Token) {
        match &comment.kind {
            TokenKind::SingleLineComment(comment) => {
                self.push_str("//");
                self.push_str(comment);
                self.push_newline();
            }
            TokenKind::MultiLineComment(comment) => {
                self.push_str("/*");
                self.push_str(comment);
                self.push_str("*/");
                self.push_newline();
            }
            _ => {}
        }
    }

    pub(crate) fn generate_comments(&mut self, comments: &[Token]) {
        for comment in comments {
            self.push_indent();
            self.generate_comment(comment);
        }
    }

    // TODO: This is used for destructuring, we might not want to have destructuring in the
    //       language and use pattern matching instead.
    pub(crate) fn generate_pat(&mut self, pattern: &hir::Pat) {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident) => {
                let var_name = self.current_scope().declare_variable(ident.as_str());
                self.push_str(&var_name);
                self.current_scope()
                    .declare_variable_alias(ident.as_str(), &var_name);
            }
            hir::PatKind::Enum(_path, _enum_pattern) => {
                todo!("enum extraction outside of function parameters is not implemented yet.")
            }
            hir::PatKind::Literal(literal) => self.generate_literal(literal),
            hir::PatKind::List(patterns) => {
                self.push_char('[');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_pat(pattern);
                }
                self.push_char(']');
            }
            hir::PatKind::Rest(pattern) => {
                self.push_str("...");
                self.generate_pat(pattern);
            }
            hir::PatKind::Wildcard => {}
        }
    }

    pub(crate) fn needs_semicolon(&self, expr: Option<&hir::Expr>) -> bool {
        match expr.map(|expr| &expr.kind) {
            Some(hir::ExprKind::Block(..) | hir::ExprKind::Match(..)) => false,
            Some(hir::ExprKind::IfElse(expr, then_branch, else_branches)) => {
                self.should_render_if_else_as_ternary(expr, then_branch, else_branches)
            }
            _ => true,
        }
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }
}
