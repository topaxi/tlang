use tlang_memory::value::{TlangValue, object::TlangObjectKind};

/// Trait for objects that can be traced by the garbage collector
pub trait GcTrace {
    /// Mark all references this object holds as reachable
    fn trace(&self, tracer: &mut dyn GcTracer);
}

/// Trait for providing tracing functionality to the GC
pub trait GcTracer {
    /// Mark an object as reachable during GC tracing
    fn mark_object(&mut self, handle: usize);
    
    /// Mark a TlangValue as reachable, handling both primitives and objects
    fn mark_value(&mut self, value: TlangValue) {
        if let Some(object_id) = value.get_object_id() {
            self.mark_object(object_id);
        }
    }
}

/// Marker trait for types that can be stored in GC-managed memory
pub trait Traceable: GcTrace + std::fmt::Debug {}

// Implement GcTrace for TlangObjectKind to trace object references
impl GcTrace for TlangObjectKind {
    fn trace(&self, tracer: &mut dyn GcTracer) {
        match self {
            TlangObjectKind::Fn(_) => {
                // Function objects don't contain GC references
            }
            TlangObjectKind::NativeFn => {
                // Native functions don't contain GC references
            }
            TlangObjectKind::String(_) => {
                // Strings don't contain GC references
            }
            TlangObjectKind::Struct(s) => {
                // Trace all values in the struct
                for value in s.values() {
                    tracer.mark_value(*value);
                }
            }
            TlangObjectKind::Enum(e) => {
                // Trace all field values in the enum
                for value in &e.field_values {
                    tracer.mark_value(*value);
                }
            }
            TlangObjectKind::Slice(slice) => {
                // Trace the underlying object the slice references
                tracer.mark_value(slice.of());
            }
            TlangObjectKind::Closure(closure) => {
                // Trace all values captured in the closure's scope stack
                for _scope in &closure.scope_stack {
                    // Note: This would need access to scope's memory to trace values
                    // For now, we'll mark this as a TODO for closure tracing
                    log::warn!("Closure GC tracing not yet implemented for scope");
                }
            }
        }
    }
}

impl Traceable for TlangObjectKind {}

// Implement GcTrace for TlangValue collections
impl GcTrace for Vec<TlangValue> {
    fn trace(&self, tracer: &mut dyn GcTracer) {
        for value in self {
            tracer.mark_value(*value);
        }
    }
}

impl Traceable for Vec<TlangValue> {}