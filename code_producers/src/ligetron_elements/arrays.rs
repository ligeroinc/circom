
use super::stack::*;
use super::types::*;
use super::wasm::*;


/// Generates loading of pointer to array element at constant index to WASM stack
pub fn gen_load_array_element_ptr_to_wasm_stack_const(frame: &mut CircomStackFrame,
                                                      arr: &dyn CircomValueRef,
                                                      index: usize) {
    let arr_type = arr.xtype(frame).clone();
    let byte_offset = match arr_type {
        CircomValueType::FRArray(_size) => CircomValueType::FR.size() * index,
        _ => {
            panic!("invalid type for loading array element");
        }
    };

    arr.gen_load_ptr_with_offset_to_wasm_stack(frame, byte_offset);
}

/// Generates loading of pointer to array element at dynamic index onto WASM stack
pub fn gen_load_array_element_ptr_to_wasm_stack_dyn(frame: &mut CircomStackFrame,
                                                    arr: &dyn CircomValueRef,
                                                    index_add: usize) {
    let arr_type = arr.xtype(frame).clone();
    let element_size = match arr_type {
        CircomValueType::FRArray(_size) => CircomValueType::FR.size(),
        _ => {
            panic!("invalid type for loading array element");
        }
    };

    // calculating byte offset from index located on WASM stack
    if index_add != 0 {
        frame.func.borrow_mut().gen_const(WASMType::I32, index_add as i64);
        frame.func.borrow_mut().gen_add(WASMType::I32);
    }

    frame.func.borrow_mut().gen_const(WASMType::I32, element_size as i64);
    frame.func.borrow_mut().gen_mul(WASMType::I32);

    // calculating value address
    arr.gen_load_ptr_to_wasm_stack(frame);
    frame.func.borrow_mut().gen_add(WASMType::PTR);
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
            CircomValueType::FRArray(_size) => {
                return CircomValueType::FR;
            }
            _ => {
                panic!("element of array of non array type is invalid");
            }
        }
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        gen_load_array_element_ptr_to_wasm_stack_const(frame, self.arr.as_ref(), self.index);
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
            CircomValueType::FRArray(_size) => {
                return CircomValueType::FRArray(self.size);
            }
            _ => {
                panic!("element of array of non array type is invalid");
            }
        }
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        gen_load_array_element_ptr_to_wasm_stack_const(frame, self.arr.as_ref(), self.index);
    }
}
