use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir;
use tlang_hir::visit::scoped_visitor::ScopedVisitor;

use crate::HirPass;
use crate::hir_opt::HirOptGroup;

use self::scope::Scope;

mod scope;

pub struct SymbolResolution(HirOptGroup);

impl SymbolResolution {
    pub fn new() -> Self {
        let ctx = Rc::new(RefCell::new(SymbolResolutionContext::default()));

        Self(HirOptGroup::new(
            std::any::type_name::<Self>(),
            vec![
                Box::new(DeclarationCollector::new(ctx.clone())),
                Box::new(IdentifierResolver::new(ctx.clone())),
            ],
        ))
    }
}

impl Default for SymbolResolution {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPass for SymbolResolution {
    fn optimize_hir(&mut self, hir: &mut hir::LowerResult) -> bool {
        self.0.optimize_hir(hir)
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
    fn optimize_hir(&mut self, _hir: &mut hir::LowerResult) -> bool {
        false
    }
}

impl<'hir> ScopedVisitor<'hir> for DeclarationCollector {
    type ScopeHandle = hir::HirId;

    fn push_scope(&mut self, hir_id: hir::HirId) -> Option<Self::ScopeHandle> {
        self.ctx.borrow_mut().scopes.push(Scope::new());

        Some(hir_id)
    }

    fn pop_scope(&mut self, _scope_handle: Self::ScopeHandle) {
        self.ctx.borrow_mut().scopes.pop();
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, _parent_scope: Self::ScopeHandle) {
        let res = self
            .ctx
            .borrow_mut()
            .lookup(&path.to_string())
            .map_or(hir::Res::Unknown, |binding| binding.res());

        path.set_res(res);
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
    fn optimize_hir(&mut self, _hir: &mut hir::LowerResult) -> bool {
        false
    }
}
