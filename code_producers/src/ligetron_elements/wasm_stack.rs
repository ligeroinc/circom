
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// Value located on WASM stack
#[derive(Clone)]
pub struct WASMStackValue {
    tp: WASMType
}

impl WASMStackValue {
    /// Creates new value
    pub fn new(xtype: WASMType) -> WASMStackValue {
        return WASMStackValue {
            tp: xtype
        };
    }
}

impl CircomValueRef for WASMStackValue {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("WASM STACK {}", self.tp.to_string());
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return self.dump_ref(frame);
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return CircomValueType::WASM(super::WASMType::I32);
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return true;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  _inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
//        panic!("can't load pointer to value located on WASM stack");
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              _inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
//        panic!("value is already located on WASM stack");
    }
}



/// Dynamic pointer value located on WASM stack as PTR
#[derive(Clone)]
pub struct WASMStackAddress {
    tp: CircomValueType
}

impl WASMStackAddress {
    /// Creates new value
    pub fn new(tp: CircomValueType) -> WASMStackAddress {
        return WASMStackAddress {
            tp: tp
        };
    }
}

impl CircomValueRef for WASMStackAddress {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("WASM STACK ADDRESS FR");
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return self.dump_ref(frame);
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return self.tp.clone();
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return true;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  _inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
//        panic!("pointer value is already located on WASM stack");
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              _inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
        panic!("value is already located on WASM stack");
    }
}
