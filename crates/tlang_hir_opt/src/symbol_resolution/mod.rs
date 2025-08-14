use std::cell::RefCell;
use std::rc::Rc;

use log::{debug, warn};
use tlang_hir::visit::scoped_visitor::ScopedVisitor;
use tlang_hir::{Visitor, hir};
use tlang_span::HirId;

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptGroup};

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
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.0.optimize_hir(module, ctx)
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
    fn optimize_hir(&mut self, _module: &mut hir::Module, _ctx: &mut HirOptContext) -> bool {
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
            .map_or(hir::Res::default(), |binding| binding.res());

        path.set_res(res);
    }
}

pub struct IdentifierResolver {
    ctx: Rc<RefCell<SymbolResolutionContext>>,
    scopes: Vec<HirId>,
}

impl IdentifierResolver {
    fn new(ctx: Rc<RefCell<SymbolResolutionContext>>) -> Self {
        Self {
            ctx,
            scopes: vec![],
        }
    }
}

impl<'hir> Visitor<'hir> for IdentifierResolver {
    type Context = HirOptContext;

    fn enter_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            ctx.current_scope = Some(hir_id);
            self.scopes.push(hir_id);
        }
    }

    fn leave_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug_assert!(self.scopes.pop() == Some(hir_id), "Mismatched scope exit");

            ctx.current_scope = self.scopes.last().copied();
        }
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, ctx: &mut Self::Context) {
        let symbol_table = ctx
            .current_symbol_table()
            .unwrap_or_else(|| {
                panic!(
                    "No symbol table for current scope of {:?} while resolving {}.\nAvailable scopes: {:#?}",
                    ctx.current_scope,
                    path,
                    ctx.symbols.keys()
                )
            });

        let symbols = symbol_table
            .borrow()
            .get_by_name(&path.to_string())
            .iter()
            .filter_map(|s| s.hir_id)
            .collect::<Vec<_>>();

        if let Some(hir_id) = symbols.last() {
            debug!("Resolved path '{}' to {:?}", path, hir_id);

            path.res.set_hir_id(*hir_id);
        } else {
            // TODO: Builtin symbols do not have a HirId, we should handle/resolve these somehow.
            warn!(
                "No symbols found for path '{}'. Available symbols: {:#?}",
                path,
                symbol_table.borrow()
            );
        }
    }
}

impl HirPass for IdentifierResolver {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);

        false
    }
}
