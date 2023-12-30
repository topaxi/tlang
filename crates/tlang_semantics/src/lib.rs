use tlang_ast::{node::{Node, AstNode}, symbols::{SymbolTable, SymbolInfo, SymbolType}};

pub struct SemanticAnalyzer {
    root_symbol_table: SymbolTable,

    symbol_table_stack: Vec<SymbolTable>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            root_symbol_table: SymbolTable::default(),
            symbol_table_stack: Vec::new(),
        }
    }

    fn get_last_symbol_table(&self) -> SymbolTable {
        if self.symbol_table_stack.is_empty() {
            return self.root_symbol_table.clone();
        }

        self.symbol_table_stack.last().unwrap().clone()
    }

    fn get_last_symbol_table_mut(&mut self) -> &mut SymbolTable {
        if self.symbol_table_stack.is_empty() {
            return &mut self.root_symbol_table;
        }

        self.symbol_table_stack.last_mut().unwrap()
    }

    fn push_symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_table_stack.push(self.get_last_symbol_table().push());

        self.get_last_symbol_table_mut()
    }

    pub fn add_builtin_symbols(&mut self, symbols: Vec<(&str, &str, SymbolType)>) {
        for (name, alias, symbol_type) in symbols {
            self.root_symbol_table.symbols.insert(
                name.to_string(),
                SymbolInfo {
                    name: alias.to_string(),
                    symbol_type,
                },
            );
        }
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        ast.symbol_table = self.get_last_symbol_table();

        match ast.ast_node {
            AstNode::Program(ref mut nodes) => self.analyze_program(nodes),
            AstNode::Block(ref mut nodes, ref mut return_value) => {
                self.analyze_block(nodes, return_value)
            }
            AstNode::VariableDeclaration {
                ref mut name,
                ref mut value,
            } => self.analyze_variable_declaration(name, value),
            _ => {}
        }

    }

    fn analyze_program(&mut self, nodes: &mut [Node]) {
        for node in nodes {
            self.analyze(node);
        }
    }

    fn analyze_block(&mut self, nodes: &mut [Node], expr: &mut Option<Box<Node>>) {
        self.push_symbol_table();

        for node in nodes {
            self.analyze(node);
        }

        if let Some(expr) = expr {
            self.analyze(expr);
        }

        self.symbol_table_stack.pop();
    }

    fn analyze_variable_declaration(&mut self, name: &str, _expr: &mut Node) {
        let symbol_table = self.get_last_symbol_table_mut();

        let symbol_info = SymbolInfo {
            name: name.to_string(),
            symbol_type: SymbolType::Variable,
        };

        symbol_table.insert(name.to_string(), symbol_info);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_ast::{node, token::Literal};
    use tlang_parser::parser::Parser;

    macro_rules! analyze {
        ($source:expr) => {{
            let mut parser = Parser::from_source($source);
            let mut ast = parser.parse();
            let mut analyzer = SemanticAnalyzer::new();
            analyzer.analyze(&mut ast);
            ast
        }};
    }

    #[test]
    fn test_analyze_variable_declaration() {
        let ast = analyze!("let a = 1;");

        assert_eq!(
            ast.symbol_table.get("a"),
            Some(SymbolInfo {
                name: "a".to_string(),
                symbol_type: SymbolType::Variable,
            })
        );
    }
}
