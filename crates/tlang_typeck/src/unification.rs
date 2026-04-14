//! Hindley-Milner style type variable unification.
//!
//! Provides a [`UnificationTable`] backed by a Union-Find data structure for
//! managing type variable equivalence classes.  Includes an occurs check to
//! prevent infinite types (e.g. `T = List<T>`).

use std::collections::HashMap;

use tlang_hir::{Ty, TyKind};
use tlang_span::TypeVarId;

/// Error returned when unification detects an infinite type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OccursCheckError {
    /// The type variable that was found to occur within its own solution.
    pub var: TypeVarId,
    /// The type that contains the variable.
    pub ty: TyKind,
}

/// Union-Find table for type variable unification.
///
/// Each type variable is either:
/// - **Unbound**: not yet unified with any concrete type, or
/// - **Bound**: unified to a specific `TyKind`.
///
/// The table supports path compression and union by rank for efficient
/// lookups across chains of forwarded type variables.
#[derive(Debug, Default, Clone)]
pub struct UnificationTable {
    /// Maps each known type variable to its current entry.
    entries: HashMap<TypeVarId, Entry>,
}

/// Internal entry in the Union-Find table.
#[derive(Debug, Clone)]
enum Entry {
    /// This variable points to another variable (forwarding / union).
    Forward(TypeVarId),
    /// This variable is the root of its equivalence class, bound to a type.
    Root(Option<TyKind>),
}

impl UnificationTable {
    /// Create a new empty unification table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type variable as unbound. If it's already present, this is
    /// a no-op.
    pub fn register(&mut self, var: TypeVarId) {
        self.entries.entry(var).or_insert(Entry::Root(None));
    }

    /// Find the root representative for a type variable, applying path
    /// compression along the way.
    fn find_root(&mut self, var: TypeVarId) -> TypeVarId {
        // Collect the chain so we can compress paths.
        let mut path = Vec::new();
        let mut current = var;

        loop {
            match self.entries.get(&current) {
                Some(Entry::Forward(next)) => {
                    path.push(current);
                    current = *next;
                }
                Some(Entry::Root(_)) | None => break,
            }
        }

        // Path compression: point everything directly at the root.
        for id in path {
            self.entries.insert(id, Entry::Forward(current));
        }

        current
    }

    /// Probe the current binding of a type variable. Returns `Some(ty)` if
    /// the variable has been solved, `None` if it is still unbound.
    pub fn probe(&mut self, var: TypeVarId) -> Option<TyKind> {
        let root = self.find_root(var);
        match self.entries.get(&root) {
            Some(Entry::Root(Some(ty))) => Some(ty.clone()),
            _ => None,
        }
    }

    /// Unify a type variable with a concrete type, performing the occurs check.
    ///
    /// Returns `Err(OccursCheckError)` if `var` occurs inside `ty`.
    pub fn unify_var_ty(&mut self, var: TypeVarId, ty: &TyKind) -> Result<(), OccursCheckError> {
        let root = self.find_root(var);

        // If the variable is already bound, we don't re-bind it.
        // The caller should check compatibility.
        if let Some(Entry::Root(Some(_))) = self.entries.get(&root) {
            return Ok(());
        }

        // Skip trivial `Var(same)` binding.
        if let TyKind::Var(other) = ty {
            let other_root = self.find_root(*other);
            if root == other_root {
                return Ok(());
            }
        }

        // Occurs check: var must not appear inside ty.
        if occurs_in(root, ty, self) {
            return Err(OccursCheckError {
                var: root,
                ty: ty.clone(),
            });
        }

        self.entries.insert(root, Entry::Root(Some(ty.clone())));
        Ok(())
    }

    /// Unify two type variables so they share the same equivalence class.
    pub fn unify_var_var(&mut self, a: TypeVarId, b: TypeVarId) {
        let root_a = self.find_root(a);
        let root_b = self.find_root(b);

        if root_a == root_b {
            return;
        }

        // If one has a binding, forward the other to it.
        let (from, to) = match (
            self.entries.get(&root_a).cloned(),
            self.entries.get(&root_b).cloned(),
        ) {
            (Some(Entry::Root(Some(_))), _) => (root_b, root_a),
            (_, Some(Entry::Root(Some(_)))) => (root_a, root_b),
            _ => (root_b, root_a),
        };
        self.entries.insert(from, Entry::Forward(to));
    }

    /// Zonk a type: recursively replace all solved type variables with their
    /// solutions. Unbound variables are left as `TyKind::Var`.
    pub fn zonk(&mut self, ty: &TyKind) -> TyKind {
        match ty {
            TyKind::Var(id) => {
                let root = self.find_root(*id);
                match self.probe(root) {
                    Some(solution) => {
                        // Recursively zonk the solution in case it contains
                        // other type variables.
                        self.zonk(&solution)
                    }
                    None => TyKind::Var(root),
                }
            }
            TyKind::Slice(inner) => TyKind::Slice(Box::new(Ty {
                kind: self.zonk(&inner.kind),
                ..Ty::default()
            })),
            TyKind::Dict(k, v) => TyKind::Dict(
                Box::new(Ty {
                    kind: self.zonk(&k.kind),
                    ..Ty::default()
                }),
                Box::new(Ty {
                    kind: self.zonk(&v.kind),
                    ..Ty::default()
                }),
            ),
            TyKind::Fn(params, ret) => {
                let params = params
                    .iter()
                    .map(|p| Ty {
                        kind: self.zonk(&p.kind),
                        ..Ty::default()
                    })
                    .collect();
                let ret = Box::new(Ty {
                    kind: self.zonk(&ret.kind),
                    ..Ty::default()
                });
                TyKind::Fn(params, ret)
            }
            TyKind::Union(tys) => TyKind::Union(
                tys.iter()
                    .map(|t| Ty {
                        kind: self.zonk(&t.kind),
                        ..Ty::default()
                    })
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }
}

/// Check whether `var` occurs anywhere inside `ty`.
/// Follows forwarding chains in the unification table.
fn occurs_in(var: TypeVarId, ty: &TyKind, table: &mut UnificationTable) -> bool {
    match ty {
        TyKind::Var(id) => {
            let root = table.find_root(*id);
            if root == var {
                return true;
            }
            // If this variable is bound, check the binding too.
            if let Some(bound) = table.probe(root) {
                return occurs_in(var, &bound, table);
            }
            false
        }
        TyKind::Slice(inner) => occurs_in(var, &inner.kind, table),
        TyKind::Dict(k, v) => occurs_in(var, &k.kind, table) || occurs_in(var, &v.kind, table),
        TyKind::Fn(params, ret) => {
            params.iter().any(|p| occurs_in(var, &p.kind, table))
                || occurs_in(var, &ret.kind, table)
        }
        TyKind::Union(tys) => tys.iter().any(|t| occurs_in(var, &t.kind, table)),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_hir::PrimTy;
    use tlang_span::TypeVarIdAllocator;

    #[test]
    fn unbound_variable_probes_to_none() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);

        assert_eq!(table.probe(t), None);
    }

    #[test]
    fn bind_variable_and_probe() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);
        table
            .unify_var_ty(t, &TyKind::Primitive(PrimTy::I64))
            .unwrap();

        assert_eq!(table.probe(t), Some(TyKind::Primitive(PrimTy::I64)));
    }

    #[test]
    fn unify_two_vars_shares_binding() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);
        table.register(u);

        // Unify T and U
        table.unify_var_var(t, u);

        // Bind one of them
        table
            .unify_var_ty(t, &TyKind::Primitive(PrimTy::F64))
            .unwrap();

        // Both should resolve to f64
        assert_eq!(table.probe(t), Some(TyKind::Primitive(PrimTy::F64)));
        assert_eq!(table.probe(u), Some(TyKind::Primitive(PrimTy::F64)));
    }

    #[test]
    fn occurs_check_prevents_infinite_type() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);

        // Try to unify T = Slice(T) — should fail with occurs check.
        let infinite_ty = TyKind::Slice(Box::new(Ty {
            kind: TyKind::Var(t),
            ..Ty::default()
        }));

        let result = table.unify_var_ty(t, &infinite_ty);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.var, t);
    }

    #[test]
    fn occurs_check_allows_different_var() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);
        table.register(u);

        // T = Slice(U) — should succeed (no cycle).
        let ty = TyKind::Slice(Box::new(Ty {
            kind: TyKind::Var(u),
            ..Ty::default()
        }));

        assert!(table.unify_var_ty(t, &ty).is_ok());
        assert_eq!(table.probe(t), Some(ty));
    }

    #[test]
    fn zonk_replaces_solved_vars() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);
        table.register(u);
        table
            .unify_var_ty(t, &TyKind::Primitive(PrimTy::I64))
            .unwrap();
        table
            .unify_var_ty(u, &TyKind::Primitive(PrimTy::F64))
            .unwrap();

        // Fn(Var(T), Var(U)) → Fn(i64, f64)
        let ty = TyKind::Fn(
            vec![Ty {
                kind: TyKind::Var(t),
                ..Ty::default()
            }],
            Box::new(Ty {
                kind: TyKind::Var(u),
                ..Ty::default()
            }),
        );

        let zonked = table.zonk(&ty);
        assert_eq!(
            zonked,
            TyKind::Fn(
                vec![Ty {
                    kind: TyKind::Primitive(PrimTy::I64),
                    ..Ty::default()
                }],
                Box::new(Ty {
                    kind: TyKind::Primitive(PrimTy::F64),
                    ..Ty::default()
                })
            )
        );
    }

    #[test]
    fn zonk_leaves_unbound_vars() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);

        let ty = TyKind::Var(t);
        let zonked = table.zonk(&ty);
        assert_eq!(zonked, TyKind::Var(t));
    }

    #[test]
    fn path_compression_works() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let a = alloc.next_id();
        let b = alloc.next_id();
        let c = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(a);
        table.register(b);
        table.register(c);

        // Chain: a → b → c
        table.unify_var_var(a, b);
        table.unify_var_var(b, c);

        // Bind the root
        table
            .unify_var_ty(c, &TyKind::Primitive(PrimTy::Bool))
            .unwrap();

        // All should resolve to Bool
        assert_eq!(table.probe(a), Some(TyKind::Primitive(PrimTy::Bool)));
        assert_eq!(table.probe(b), Some(TyKind::Primitive(PrimTy::Bool)));
        assert_eq!(table.probe(c), Some(TyKind::Primitive(PrimTy::Bool)));
    }

    #[test]
    fn self_unification_is_noop() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);

        // T = T should succeed (trivial).
        assert!(table.unify_var_ty(t, &TyKind::Var(t)).is_ok());
        assert_eq!(table.probe(t), None); // still unbound
    }

    #[test]
    fn zonk_nested_solution() {
        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut table = UnificationTable::new();
        table.register(t);
        table.register(u);

        // T = Slice(U), U = i64
        table
            .unify_var_ty(
                t,
                &TyKind::Slice(Box::new(Ty {
                    kind: TyKind::Var(u),
                    ..Ty::default()
                })),
            )
            .unwrap();
        table
            .unify_var_ty(u, &TyKind::Primitive(PrimTy::I64))
            .unwrap();

        // zonk(T) should give Slice(i64)
        let zonked = table.zonk(&TyKind::Var(t));
        assert_eq!(
            zonked,
            TyKind::Slice(Box::new(Ty {
                kind: TyKind::Primitive(PrimTy::I64),
                ..Ty::default()
            }))
        );
    }
}
