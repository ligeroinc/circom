
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// I32 contant value
#[derive(Clone)]
pub struct ConstValue {
    val: i32
}

impl ConstValue {
    /// Constructs new constant value
    pub fn new(value: i32) -> ConstValue {
        return ConstValue {
            val: value
        };
    }

    /// Returns i32 constant value
    pub fn value(&self) -> i32 {
        return self.val;
    }
}

impl CircomValueRef for ConstValue {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("CONST i32 {}", self.val);
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
        return false;
    }

    /// Casts this value to i32 constant
    fn as_i32_const(&self) -> Option<&ConstValue> {
        return Some(self);
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  _inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
        panic!("can't load pointer to i32 constant to WASM stack");
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
        inst_gen.gen_const(WASMType::I32, self.val as i64);
    }
}


/// Value located at constant address
#[derive(Clone)]
pub struct GlobalValue {
    tp: CircomValueType,
    addr: usize,
}

impl GlobalValue {
    /// Creates new value
    pub fn new(tp: CircomValueType, addr: usize) -> GlobalValue {
        return GlobalValue {
            tp: tp,
            addr: addr
        };
    }
}

impl CircomValueRef for GlobalValue {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("GLOBAL {} {}", self.tp.to_string(), self.addr);
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
        inst_gen.gen_const(WASMType::PTR, self.addr as i64);
    }
}
