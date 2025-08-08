use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir;

use crate::HirPass;
use crate::hir_opt::HirOptGroup;

use self::scope::Scope;

mod scope;

#[derive(Default)]
pub struct SymbolResolution(HirOptGroup);

impl SymbolResolution {
    pub fn new() -> Self {
        let ctx = Rc::new(RefCell::new(SymbolResolutionContext::default()));

        Self(HirOptGroup::new(vec![
            Box::new(DeclarationCollector::new(ctx.clone())),
            Box::new(IdentifierResolver::new(ctx.clone())),
        ]))
    }
}

impl HirPass for SymbolResolution {
    fn optimize_module(&mut self, module: &mut hir::Module) -> bool {
        self.0.optimize_module(module)
    }
}

#[derive(Default)]
struct SymbolResolutionContext {
    scopes: Vec<Scope>,
}

impl SymbolResolutionContext {
    #[inline(always)]
    pub(crate) fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn has_binding(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.lookup(name).is_some())
    }

    pub(crate) fn lookup_name(&mut self, name: &str) -> String {
        self.lookup(name)
            .map_or(name.to_string(), |binding| binding.name().to_string())
    }

    pub(crate) fn lookup<'a>(&'a mut self, name: &'a str) -> Option<scope::Binding> {
        let (scope_index, binding) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, scope)| scope.lookup(name).map(|binding| (i, binding.clone())))
            .or_else(|| {
                Some((
                    usize::MAX,
                    self.scopes
                        .iter()
                        .rev()
                        .find_map(|scope| scope.lookup_definition(name))?
                        .clone(),
                ))
            })?;

        if scope_index < self.scopes.len() - 1 {
            let relative_scope_index = self.scopes.len() - 1 - scope_index;
            let slot_index = binding.res().slot_index().unwrap();
            let original_hir_id = binding
                .res()
                .hir_id()
                .expect("Upvar source binding should have a valid HirId");

            return Some(self.scopes.last_mut().unwrap().def_upvar(
                binding.name(),
                original_hir_id,
                relative_scope_index,
                slot_index,
            ));
        }

        Some(binding)
    }
    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering new scope");
        self.scopes.push(Scope::new());
        let mut result = f(self);
        result.set_locals(self.scope().locals());
        result.set_upvars(self.scope().upvars());
        debug!("Leaving scope");
        self.scopes.pop();
        result
    }
}

pub struct DeclarationCollector {
    ctx: Rc<RefCell<SymbolResolutionContext>>,
}

impl DeclarationCollector {
    fn new(ctx: Rc<RefCell<SymbolResolutionContext>>) -> Self {
        Self { ctx }
    }
}

impl HirPass for DeclarationCollector {
    fn optimize_module(&mut self, _module: &mut tlang_hir::hir::Module) -> bool {
        false
    }
}

pub struct IdentifierResolver {
    ctx: Rc<RefCell<SymbolResolutionContext>>,
}

impl IdentifierResolver {
    fn new(ctx: Rc<RefCell<SymbolResolutionContext>>) -> Self {
        Self { ctx }
    }
}

impl HirPass for IdentifierResolver {
    fn optimize_module(&mut self, _module: &mut tlang_hir::hir::Module) -> bool {
        false
    }
}
