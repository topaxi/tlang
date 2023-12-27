use walrus::ValType;
use walrus::ir::LocalId;
use tlang_parser::ast::Node;

pub struct Generator {
    module: walrus::Module,
}

impl Generator {
    pub fn new() -> Self {
        let mut module = walrus::Module::default();
        Self { module }
    }

    pub fn generate_node(&mut self, node: &Node) -> walrus::Module {
        match node {
            Node::VariableDeclaration { name, value } => self.generate_variable(name, value),
            Node::FunctionDeclaration { name, parameters, body } => self.generate_function(name, parameters, body),
            _ => unimplemented!(),
        }
    }

    fn generate_variable(&mut self, name: &str, value: &Node) -> walrus::Module {
        let mut builder = walrus::FunctionBuilder::new(&mut self.module.types, &[], &[]);
        let local = builder.func.locals.add(ValType::I32);
        let local_id = LocalId::new(local.index() as u32);
        let value = builder.value_type(value);
        builder.local_set(local_id, value);
        builder.build();

        self.module
    }

    fn generate_function(&mut self, name: &str, params: &[Node], body: &Node) -> walrus::Module {
        todo!()
    }
}