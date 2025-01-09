
use super::memory_stack::*;
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// Temporary value located on memory stack
#[derive(Clone)]
pub struct TemporaryStackValue {
    /// Type of value
    type_: CircomValueType,

    /// Reference to corresponding memory stack value
    mem_stack_val_: MemoryStackValueRef,
}

impl TemporaryStackValue {
    /// Creates new value
    pub fn new(tp: CircomValueType, mem_val: MemoryStackValueRef) -> TemporaryStackValue {
        return TemporaryStackValue {
            type_: tp,
            mem_stack_val_: mem_val
        }
    }

    /// Returns value type
    pub fn tp(&self) -> &CircomValueType {
        return &self.type_;
    }

    /// Returns reference to corresponding memory stack value
    pub fn mem_stack_val(&self) -> &MemoryStackValueRef {
        return &self.mem_stack_val_;
    }

    /// Dumps value to string
    pub fn dump(&self) -> String {
        return format!("{} {}", self.type_.to_string(), self.mem_stack_val_.dump(true));
    }

    /// Dumps reference to value to string
    pub fn dump_ref(&self) -> String {
        return self.mem_stack_val_.dump_ref();
    }
}

impl CircomValueRef for TemporaryStackValue {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK #{}", self.dump_ref());
    }

    /// Dumps value to string
    fn dump(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK {}", self.dump());
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return self.tp().clone();
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Returns true if value is located on memory stack
    fn is_mem_stack(&self) -> bool {
        return true;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        frame.mem_frame.borrow_mut().gen_load_value_addr_igen(self.mem_stack_val(), 0, inst_gen);
    }

    /// Generates loading of value ptr with specified constant offset onto WASM stack
    fn gen_load_ptr_with_offset_to_wasm_stack(&self,
                                              inst_gen: &mut InstructionGenerator,
                                              frame: &CircomStackFrame,
                                              byte_offset: usize) {
        frame.mem_frame.borrow_mut().gen_load_value_addr_igen(self.mem_stack_val(),
                                                              byte_offset,
                                                              inst_gen);
    }

    /// Performs additional tasks for deallocating value. Default implementation does nothing
    fn deallocate(&self, inst_gen: &mut InstructionGenerator, frame: &CircomStackFrame) {
        frame.clear_fr_values(inst_gen, self);
    }
}


/// Reference to temporary stack value
#[derive(Clone)]
pub struct TemporaryStackValueRef {
    /// Type of value
    type_: CircomValueType,

    /// Reference to corresponding memory stack value
    mem_stack_val_: MemoryStackValueRef,
}

impl TemporaryStackValueRef {
    /// Creates new value
    pub fn new(tp: CircomValueType, mem_val: MemoryStackValueRef) -> TemporaryStackValueRef {
        return TemporaryStackValueRef {
            type_: tp,
            mem_stack_val_: mem_val
        }
    }

    /// Returns value type
    pub fn tp(&self) -> &CircomValueType {
        return &self.type_;
    }

    /// Returns reference to corresponding memory stack value
    pub fn mem_stack_val(&self) -> &MemoryStackValueRef {
        return &self.mem_stack_val_;
    }

    /// Dumps value to string
    pub fn dump(&self) -> String {
        return format!("{} {}", self.type_.to_string(), self.mem_stack_val_.dump(true));
    }

    /// Dumps reference to value to string
    pub fn dump_ref(&self) -> String {
        return self.mem_stack_val_.dump_ref();
    }
}

impl CircomValueRef for TemporaryStackValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK REF #{}", self.dump_ref());
    }

    /// Dumps value to string
    fn dump(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK REF {}", self.dump());
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return self.tp().clone();
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        frame.mem_frame.borrow_mut().gen_load_value_addr_igen(self.mem_stack_val(), 0, inst_gen);
    }

    /// Generates loading of value ptr with specified constant offset onto WASM stack
    fn gen_load_ptr_with_offset_to_wasm_stack(&self,
                                              inst_gen: &mut InstructionGenerator,
                                              frame: &CircomStackFrame,
                                              byte_offset: usize) {
        frame.mem_frame.borrow_mut().gen_load_value_addr_igen(self.mem_stack_val(),
                                                              byte_offset,
                                                              inst_gen);
    }
}
