/// Trait for objects that can be traced by the garbage collector
pub trait GcTrace {
    /// Mark all references this object holds as reachable
    fn trace(&self, tracer: &mut dyn GcTracer);
}

/// Trait for providing tracing functionality to the GC
pub trait GcTracer {
    /// Mark an object as reachable during GC tracing
    fn mark_object(&mut self, handle: usize);
}

/// Marker trait for types that can be stored in GC-managed memory
pub trait Traceable: GcTrace + std::fmt::Debug {}