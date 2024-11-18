
use super::wasm::*;
use super::memory_stack::*;


/// Reference to a generic Circom value
#[derive(Clone)]
pub enum CircomValueRef {
    /// Circom value referenced by memory address located in WASM variable
    MemoryRefWASMLocal(WASMLocalVariableRef),

    /// Circom value located in local variable in memory stack
    MemoryStackLocal(MemoryStackLocalRef),
}

