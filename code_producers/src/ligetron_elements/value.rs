
use super::arrays::*;
use super::constants::*;
use super::stack::*;
use super::types::*;
use super::wasm::*;
use super::CircomComponent;


pub trait CircomValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef>;

    /// Dumps reference information to string in short form
    fn dump_ref(&self, frame: &CircomStackFrame) -> String;

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String;

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType;

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool;

    /// Returns true if value is located on memory stack
    fn is_mem_stack(&self) -> bool {
        return false;
    }

    /// Casts this value to array index
    fn as_array_index(&self) -> Option<&ArrayIndex> {
        return None;
    }

    /// Casts this value to i32 constant
    fn as_i32_const(&self) -> Option<&ConstValue> {
        return None;
    }

    /// Casts this value to compoment
    fn as_component(&self) -> Option<&CircomComponent> {
        return None;
    }

    /// Generates loading of value ptr onto WASM stack for specified instruction generator
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame);

    /// Generates loading of value ptr with specified constant offset onto WASM stack
    /// for specified instruction generator
    fn gen_load_ptr_with_offset_to_wasm_stack(&self,
                                              inst_gen: &mut InstructionGenerator,
                                              frame: &CircomStackFrame,
                                              byte_offset: usize) {
        self.gen_load_ptr_to_wasm_stack(inst_gen, frame);

        if byte_offset != 0 {
            inst_gen.gen_const(WASMType::I32, byte_offset as i64);
            inst_gen.gen_add(WASMType::PTR);
        }
    }

    /// Generates loading of value onto WASM stack. Default implementation loads value ptr
    /// and then generates additional load.
    fn gen_load_to_wasm_stack(&self,
                              inst_gen: &mut InstructionGenerator,
                              frame: &CircomStackFrame) {
        let wasm_type = match self.xtype(frame) {
            CircomValueType::WASM(wasm_type) => wasm_type,
            _ => {
                panic!("Fr value can't be loaded by value to WASM stack");
            }
        };

        self.gen_load_ptr_to_wasm_stack(inst_gen, frame);
        inst_gen.gen_load(wasm_type);
    }

    /// Performs additional tasks for deallocating value. Default implementation does nothing.
    fn deallocate(&self, _inst_gen: &mut InstructionGenerator, _frame: &CircomStackFrame) {
    }
}
