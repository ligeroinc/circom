
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// Value referenced by pointer stored in WASM local variable
#[derive(Clone)]
pub struct WASMLocalVariableValuePtrRef {
    var: WASMLocalVariableRef,
    tp: CircomValueType
}

impl WASMLocalVariableValuePtrRef {
    /// Creates new value
    pub fn new(var: WASMLocalVariableRef, tp: CircomValueType) -> WASMLocalVariableValuePtrRef {
        return WASMLocalVariableValuePtrRef {
            var: var,
            tp: tp
        };
    }
}

impl CircomValueRef for WASMLocalVariableValuePtrRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("REF WASM LOCAL {}", frame.func.borrow().gen_local_ref(&self.var));
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
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
        inst_gen.gen_local_get(&self.var);
    }
}

/// Value stored in WASM local variable
#[derive(Clone)]
pub struct WASMLocalVariableValueRef {
    var: WASMLocalVariableRef
}

impl WASMLocalVariableValueRef {
    /// Creates new value
    pub fn new(var: WASMLocalVariableRef) -> WASMLocalVariableValueRef {
        return WASMLocalVariableValueRef {
            var: var
        };
    }
}

impl CircomValueRef for WASMLocalVariableValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("WASM LOCAL {}", frame.func.borrow().gen_local_ref(&self.var));
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return self.dump_ref(frame);
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        let (_, tp) = frame.func.borrow().local(&self.var);
        return CircomValueType::WASM(tp);
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  _inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
        panic!("can't load pointer to value located in WASM local")
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
        inst_gen.gen_local_get(&self.var);
    }
}
