use crate::node;

pub trait Visitor<'ast>: Sized {
    // TODO: Create structs for each node type.
    fn visit_module(&mut self, _program: &'ast node::Node) {}
}
