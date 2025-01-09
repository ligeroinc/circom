
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// Generates loading of pointer to array element at constant index to WASM stack
pub fn gen_load_array_element_ptr_to_wasm_stack_const(frame: &CircomStackFrame,
                                                      arr: &dyn CircomValueRef,
                                                      index: usize,
                                                      inst_gen: &mut InstructionGenerator) {
    let arr_type = arr.xtype(frame).clone();
    let byte_offset = match arr_type {
        CircomValueType::Array(tp, _size) => tp.size() * index,
        _ => {
            panic!("invalid type for loading array element");
        }
    };

    arr.gen_load_ptr_with_offset_to_wasm_stack(inst_gen, frame, byte_offset);
}

/// Generates loading of pointer to array element at dynamic index onto WASM stack
pub fn gen_load_array_element_ptr_to_wasm_stack_dyn(frame: &CircomStackFrame,
                                                    arr: &dyn CircomValueRef,
                                                    index_add: usize,
                                                    inst_gen: &mut InstructionGenerator) {
    let arr_type = arr.xtype(frame).clone();
    let element_size = match arr_type {
        CircomValueType::Array(tp, _size) => tp.size(),
        _ => {
            panic!("invalid type for loading array element");
        }
    };

    // calculating byte offset from index located on WASM stack
    if index_add != 0 {
        inst_gen.gen_const(WASMType::I32, index_add as i64);
        inst_gen.gen_add(WASMType::I32);
    }

    inst_gen.gen_const(WASMType::I32, element_size as i64);
    inst_gen.gen_mul(WASMType::I32);

    // calculating value address
    arr.gen_load_ptr_to_wasm_stack(inst_gen, frame);
    inst_gen.gen_add(WASMType::PTR);
}


/// Reference to array element
#[derive(Clone)]
pub struct ArrayElementRef {
    /// Reference to array
    arr: Box<dyn CircomValueRef>,

    /// Element index
    index: usize
}

impl ArrayElementRef {
    /// Creates new array element reference
    pub fn new(arr: Box<dyn CircomValueRef>, index: usize) -> ArrayElementRef {
        return ArrayElementRef {
            arr: arr,
            index
        };
    }    
}

impl CircomValueRef for ArrayElementRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("({})[{}]", self.arr.dump_ref(frame), self.index);
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return format!("{}, FR", self.dump_ref(frame));
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        let arr_type = self.arr.xtype(frame);
        match arr_type {
            CircomValueType::Array(tp, _size) => {
                return *tp;
            }
            _ => {
                panic!("element of array of non array type is invalid");
            }
        }
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return self.arr.is_wasm_stack();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        gen_load_array_element_ptr_to_wasm_stack_const(frame,
                                                       self.arr.as_ref(),
                                                       self.index,
                                                       inst_gen);
    }
}


/// Reference to array slice
#[derive(Clone)]
pub struct ArraySliceRef {
    /// Reference to array
    arr: Box<dyn CircomValueRef>,

    /// Element offset
    index: usize,

    /// Size of slice
    size: usize
}

impl ArraySliceRef {
    /// Creates new array element reference
    pub fn new(arr: Box<dyn CircomValueRef>, index: usize, size: usize) -> ArraySliceRef {
        return ArraySliceRef {
            arr: arr,
            index,
            size: size
        };
    }    
}

impl CircomValueRef for ArraySliceRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("({})[{}, {})",
                       self.arr.dump_ref(frame),
                       self.index,
                       self.index + self.size);
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return format!("{}, FR", self.dump_ref(frame));
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        let arr_type = self.arr.xtype(frame);
        match arr_type {
            CircomValueType::Array(tp, _size) => {
                return CircomValueType::Array(tp, self.size);
            }
            _ => {
                panic!("element of array of non array type is invalid");
            }
        }
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return self.arr.is_wasm_stack();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        gen_load_array_element_ptr_to_wasm_stack_const(frame,
                                                       self.arr.as_ref(),
                                                       self.index,
                                                       inst_gen);
    }
}


/// Array index value located on WASM stack with constant offset
#[derive(Clone)]
pub struct ArrayIndex {
    offs: i32
}

impl ArrayIndex {
    /// Creates new array index
    pub fn new(offset: i32) -> ArrayIndex {
        return ArrayIndex {
            offs: offset
        };
    }

    /// Returns constant offset
    pub fn offset(&self) -> i32 {
        return self.offs;
    }
}

impl CircomValueRef for ArrayIndex {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("INDEX WSTACK + {}", self.offs);
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

    /// Casts this value to array index value
    fn as_array_index(&self) -> Option<&ArrayIndex> {
        return Some(self);
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  _inst_gen: &mut InstructionGenerator,
                                  _frame: &CircomStackFrame) {
        panic!("can't load pointer to i32 array index to WASM stack");
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
        // NOTE: we assume that this function is called with correct WASM stack state
        // and required value is located on top of stack
        inst_gen.gen_const(WASMType::I32, self.offs as i64);
        inst_gen.gen_add(WASMType::I32);
    }
}


/// Returns array element reference
pub fn array_element(arr: Box<dyn CircomValueRef>, index: usize) -> Box<dyn CircomValueRef> {
    return Box::new(ArrayElementRef::new(arr, index));
}

/// Returns array slice reference
pub fn array_slice(arr: Box<dyn CircomValueRef>, index: usize, size: usize) -> Box<dyn CircomValueRef> {
    return Box::new(ArraySliceRef::new(arr, index, size));
}
