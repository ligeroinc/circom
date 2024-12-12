
use crate::ligetron_elements::wasm;

use super::memory_stack::*;
use super::value::*;
use super::wasm::*;
use super::CircomValueType;

use std::cell::RefCell;
use std::rc::Rc;


/// Circom local variable
struct CircomLocalVariable {
    /// Variable name
    name: String,

    /// Variable type
    type_: CircomValueType,

    /// Pointer to memory stack local allocated for variable
    loc: MemoryStackLocalRef
}

impl CircomLocalVariable {
    /// Creates new variable
    pub fn new(name: String, tp: CircomValueType, loc: MemoryStackLocalRef) -> CircomLocalVariable {
        return CircomLocalVariable {
            name: name,
            type_: tp,
            loc: loc
        };
    }
}


/// Reference to local variable
#[derive(Clone)]
pub struct CircomLocalVariableRef {
    idx: usize
}

impl CircomLocalVariableRef {
    /// Creates reference to local variable
    fn new(idx: usize) -> CircomLocalVariableRef {
        return CircomLocalVariableRef {
            idx: idx
        };
    }
}


/// Circom function parameter
pub struct CircomParameter {
    /// Parameter name
    name: String,

    /// Parameter type
    type_: CircomValueType,

    /// Reference to WASM local containing pointer to parameter memory
    wasm_loc: WASMLocalVariableRef
}

impl CircomParameter {
    /// Creates new parameter
    fn new(name: String, tp: CircomValueType, wasm_loc: WASMLocalVariableRef) -> CircomParameter {
        return CircomParameter {
            name: name,
            type_: tp,
            wasm_loc: wasm_loc
        };
    }
}


/// Reference to Circom function parameter
#[derive(Clone)]
pub struct CircomParameterRef {
    idx: usize
}

impl CircomParameterRef {
    /// Creates reference to function parameter
    fn new(idx: usize) -> CircomParameterRef {
        return CircomParameterRef {
            idx: idx
        };
    }
}


/// Function return value
struct CircomReturnValue {
    /// Return value type
    type_: CircomValueType,

    /// Reference to WASM local containing pointer to result memory
    wasm_loc: WASMLocalVariableRef
}

impl CircomReturnValue {
    /// Creates new return value
    fn new(tp: CircomValueType, wasm_loc: WASMLocalVariableRef) -> CircomReturnValue {
        return CircomReturnValue {
            type_: tp,
            wasm_loc: wasm_loc
        };
    }
}


/// Reference to Circom function return value
#[derive(Clone)]
pub struct CircomReturnValueRef {
    idx: usize
}

impl CircomReturnValueRef {
    /// Creates reference to function return value
    fn new(idx: usize) -> CircomReturnValueRef {
        return CircomReturnValueRef {
            idx: idx
        };
    }
}


/// Location of value stored on Circom stack frame
#[derive(Clone)]
pub enum CircomStackValueKind {
    /// WASM constant value
    WASMConst(WASMType, i64),

    /// Memory pointer with fixed value
    MemoryPtrConst(CircomValueType, usize),

    /// Memory pointer referenced by local WASM variable
    MemoryPtrWASMLocal(CircomValueType, WASMLocalVariableRef),

    /// Pointer to local variable
    LocalVariablePtr(CircomLocalVariableRef),

    /// Pointer to subarray in array variable (var, offset, size)
    LocalVariableArrayPtr(CircomLocalVariableRef, usize, usize),

    /// Pointer to array local variable element
    LocalVariableArrayElementPtr(CircomLocalVariableRef, usize),

    /// Pointer to parameter
    ParameterPtr(CircomParameterRef),

    /// Pointer to subarray in array parameter (parameter, offset, size)
    ParameterArrayPtr(CircomParameterRef, usize, usize),

    /// Pointer to array parameter element
    ParameterArrayElementPtr(CircomParameterRef, usize),

    /// Pointer to return value
    ReturnValuePtr(CircomReturnValueRef),

    /// Pointer to subarray in array return value (ret val, offset, size)
    ReturnValueArrayPtr(CircomReturnValueRef, usize, usize),

    /// Pointer to array return value element
    ReturnValueArrayElementPtr(CircomReturnValueRef, usize),

    /// Value stored in memory stack
    MemoryStackValue(CircomValueType, MemoryStackValueRef),

    /// Pointer to value stored in memory stack
    MemoryStackValuePtr(CircomValueType, MemoryStackValueRef),

    /// Pointer to subarray in array value stored in stack
    MemoryStackValueArrayPtr(CircomValueType, MemoryStackValueRef, usize, usize),

    /// Pointer to array memeory stack value element
    MemoryStackValueArrayElementPtr(CircomValueType, MemoryStackValueRef, usize),

    /// Value stored in local WASM variable
    WASMLocal(WASMLocalVariableRef),

    /// Value stored in WASM stack
    WASMSTack(WASMType),
}

impl CircomStackValueKind {
    /// Returns true if value is a pointer
    pub fn is_ptr(&self) -> bool {
        match self {
            CircomStackValueKind::WASMConst(..) => false,
            CircomStackValueKind::MemoryPtrConst(..) => true,
            CircomStackValueKind::MemoryPtrWASMLocal(..) => true,
            CircomStackValueKind::LocalVariablePtr(..) => true,
            CircomStackValueKind::LocalVariableArrayPtr(..) => true,
            CircomStackValueKind::LocalVariableArrayElementPtr(..) => true,
            CircomStackValueKind::ParameterPtr(..) => true,
            CircomStackValueKind::ParameterArrayPtr(..) => true,
            CircomStackValueKind::ParameterArrayElementPtr(..) => true,
            CircomStackValueKind::ReturnValuePtr(..) => true,
            CircomStackValueKind::ReturnValueArrayPtr(..) => true,
            CircomStackValueKind::ReturnValueArrayElementPtr(..) => true,
            CircomStackValueKind::MemoryStackValue(..) => false,
            CircomStackValueKind::MemoryStackValuePtr(..) => true,
            CircomStackValueKind::MemoryStackValueArrayPtr(..) => false,
            CircomStackValueKind::MemoryStackValueArrayElementPtr(..) => true,
            CircomStackValueKind::WASMLocal(..) => false,
            CircomStackValueKind::WASMSTack(..) => false
        }
    }
}


/// Value located in Circom stack frame
#[derive(Clone)]
pub struct CircomStackValue {
    /// Value kind
    kind: CircomStackValueKind,

    /// Was this value deallocated
    deallocated: bool
}

impl CircomStackValue {
    /// Creates new stack value
    fn new(kind: CircomStackValueKind) -> CircomStackValue {
        return CircomStackValue {
            kind: kind,
            deallocated: false
        };
    }
}


/// Reference to Circom stack value
#[derive(Clone)]
pub struct CircomStackValueRef {
    value_rc: Rc<RefCell<CircomStackValue>>
}

impl CircomStackValueRef {
    /// Creates new reference to Circom stack value
    pub fn new(value_rc: Rc<RefCell<CircomStackValue>>) -> CircomStackValueRef {
        return CircomStackValueRef {
            value_rc: value_rc
        };
    }
}


/// Represents Circom stack frame. Contains logic for managing current logical
/// state of stack and generating code for stack values manipulation.
pub struct CircomStackFrame {
    /// Size of Circom variables, in 32bit
    size_32_bit: usize,

    /// Reference to current WASM function
    func: Rc<RefCell<WASMFunction>>,

    /// Reference to memory stack frame
    mem_frame: Rc<RefCell<MemoryStackFrame>>,

    /// Vector of local variables
    locals: Vec<CircomLocalVariable>,

    /// Vector of parameters
    params: Vec<CircomParameter>,

    /// Vector of return values
    ret_vals: Vec<CircomReturnValue>,

    /// Vector of values located on stack
    values: Vec<Rc<RefCell<CircomStackValue>>>,

    /// Start indexes of current branches to verify stack
    branch_starts: Vec<usize>
}

impl CircomStackFrame {
    /// Creates new stack frame
    pub fn new(size_32_bit: usize,
               func: Rc<RefCell<WASMFunction>>,
               mem_frame: Rc<RefCell<MemoryStackFrame>>) -> CircomStackFrame {
        return CircomStackFrame {
            size_32_bit: size_32_bit,
            func: func,
            mem_frame: mem_frame,
            locals: vec![],
            params: vec![],
            ret_vals: vec![],
            values: vec![],
            branch_starts: vec![]
        };
    }


    ////////////////////////////////////////////////////////////
    /// Local variables

    /// Creates new local variable in stack frame
    pub fn new_local_var(&mut self, name: String, tp: CircomValueType) -> CircomLocalVariableRef {
        // allocating memory stack local
        let mem_loc = self.mem_frame.borrow_mut().new_local(tp.size(self.size_32_bit));

        // creating local vairable
        let var_idx = self.locals.len();
        let loc = CircomLocalVariable::new(name, tp, mem_loc);
        self.locals.push(loc);

        return CircomLocalVariableRef::new(var_idx);
    }

    /// Returns reference to local variable with specified index
    pub fn local_var(&self, idx: usize) -> CircomLocalVariableRef {
        assert!(idx < self.locals.len());
        return CircomLocalVariableRef::new(idx);
    }

    /// Returns local variable name
    pub fn local_var_name(&self, var: &CircomLocalVariableRef) -> &String {
        return &self.locals[var.idx].name;
    }

    /// Returns local variable type
    pub fn local_var_type(&self, var: &CircomLocalVariableRef) -> &CircomValueType {
        return &self.locals[var.idx].type_;
    }

    /// Returns reference to memory stack local for local variable
    /// TODO: refactor and remove
    pub fn local_var_mem_loc(&self, var: &CircomLocalVariableRef) -> &MemoryStackLocalRef {
        return &self.locals[var.idx].loc;
    }

    /// Loads reference to local variable to stack
    pub fn load_local_var_ref(&mut self, var_ref: &CircomLocalVariableRef) {
        self.push(CircomStackValueKind::LocalVariablePtr(var_ref.clone()));
    }

    /// Loads reference to subarray of array local variable to stack
    pub fn load_local_var_subarray_ref(&mut self,
                                       var: &CircomLocalVariableRef,
                                       offset: usize,
                                       size: usize) {
        // checking parameter type
        match self.local_var_type(var) {
            CircomValueType::FRArray(sz) => {
                // checking offset and size
                if offset + size > *sz {
                    panic!("Invalid local var subarray offset and size");
                }

                self.push(CircomStackValueKind::LocalVariableArrayPtr(var.clone(), offset, size));
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }
    }

    /// Loads reference to element of array located in local variable
    pub fn load_local_var_array_element_ref(&mut self,
                                            arr: &CircomLocalVariableRef,
                                            offset: usize) {
        // checking array variable type
        match self.local_var_type(arr) {
            CircomValueType::FRArray(sz) => {
                // checking offset
                if offset  >= *sz {
                    panic!("Invalid local var array element offset");
                }

                self.push(CircomStackValueKind::LocalVariableArrayElementPtr(arr.clone(), offset));
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Parameters

    /// Creates new parameter in stack frame
    pub fn new_param(&mut self, name: String, tp: CircomValueType) -> CircomParameterRef {
        // creating WASM parameter for pointer parameter memory
        let wasm_par = self.func.borrow_mut().new_param(&name, WASMType::PTR);

        // creating parameter
        let par_idx = self.params.len();
        let par = CircomParameter::new(name, tp, wasm_par);
        self.params.push(par);

        return CircomParameterRef::new(par_idx);
    }

    /// Returns count of parameters
    pub fn params_count(&self) -> usize {
        return self.params.len();
    }

    /// Returns reference to parameter with specified index
    pub fn param(&self, idx: usize) -> CircomParameterRef {
        assert!(idx < self.params.len());
        return CircomParameterRef::new(idx);
    }

    /// Returns parameter name
    pub fn param_name(&self, par: &CircomParameterRef) -> &String {
        return &self.params[par.idx].name;
    }

    /// Returns parameter type
    pub fn param_type(&self, par: &CircomParameterRef) -> &CircomValueType {
        return &self.params[par.idx].type_;
    }

    /// Returns reference to WASM local containing address of parameter
    pub fn param_wasm_loc(&self, par: &CircomParameterRef) -> &WASMLocalVariableRef {
        return &self.params[par.idx].wasm_loc;
    }

    /// Loads reference to function parameter to stack
    pub fn load_param_ref(&mut self, par: &CircomParameterRef) {
        self.push(CircomStackValueKind::ParameterPtr(par.clone()));
    }

    /// Loads reference to subarray of function parameter to stack
    pub fn load_param_array_ref(&mut self, par: &CircomParameterRef, offset: usize, size: usize) {
        // checking parameter type
        match self.param_type(par) {
            CircomValueType::FRArray(sz) => {
                // checking offset and size
                if offset + size > *sz {
                    panic!("Invalid parameter subarray offset and size");
                }

                self.push(CircomStackValueKind::ParameterArrayPtr(par.clone(), offset, size));
            }
            _ => {
                panic!("Can't get reference to parameter subarray of non array value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Return values

    /// Creates new return value in stack frame
    pub fn new_ret_val(&mut self, tp: CircomValueType, name: Option<String>) -> CircomReturnValueRef {
        let ret_val_idx = self.ret_vals.len();

        // creating WASM parameter for pointer to return value memory
        let nm = name.unwrap_or(format!("ret_val_{}", ret_val_idx));
        let wasm_par = self.func.borrow_mut().new_param(&nm, WASMType::PTR);

        // creating return value
        let ret_val = CircomReturnValue::new(tp, wasm_par);
        self.ret_vals.push(ret_val);

        return CircomReturnValueRef::new(ret_val_idx);
    }

    /// Returns reference to return value with specified index
    pub fn ret_val(&self, idx: usize) -> CircomReturnValueRef {
        assert!(idx < self.ret_vals.len());
        return CircomReturnValueRef::new(idx);
    }

    /// Returns return value index
    pub fn ret_val_idx(&self, var: &CircomReturnValueRef) -> usize {
        return var.idx;
    }

    /// Returns return value type
    pub fn ret_val_type(&self, var: &CircomReturnValueRef) -> &CircomValueType {
        return &self.ret_vals[var.idx].type_;
    }

    /// Returns reference to WASM local containing address of return value
    pub fn ret_val_wasm_loc(&self, var: &CircomReturnValueRef) -> &WASMLocalVariableRef {
        return &self.ret_vals[var.idx].wasm_loc;
    }

    /// Loads reference to return value to stack
    pub fn load_ret_val_ref(&mut self, ret_val_ref: &CircomReturnValueRef) {
        self.push(CircomStackValueKind::ReturnValuePtr(ret_val_ref.clone()));
    }

    /// Loads reference to subarray of return value to stack
    pub fn load_ret_val_array_ref(&mut self,
                                  ret_val: &CircomReturnValueRef,
                                  offset: usize,
                                  size: usize) {
        // checking parameter type
        match self.ret_val_type(ret_val) {
            CircomValueType::FRArray(sz) => {
                // checking offset and size
                if offset + size > *sz {
                    panic!("Invalid return value subarray offset and size");
                }

                self.push(CircomStackValueKind::ReturnValueArrayPtr(
                    ret_val.clone(),
                    offset,
                    size));
            }
            _ => {
                panic!("Can't get reference to return vlaue subarray of non array value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Temporary stack values

    /// Allocates multiple values on stack. Returns references to allocated value
    pub fn alloc_stack_n(&mut self, types: &Vec<CircomValueType>) -> Vec<CircomStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let mem_stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        let mut vals = Vec::<CircomStackValueRef>::new();
        for (idx, mem_val) in mem_stack_vals.iter().enumerate() {
            let val_kind = CircomStackValueKind::MemoryStackValue(types[idx].clone(),
                                                                  mem_val.clone());
            vals.push(self.push(val_kind));
        }

        return vals;
    }

    /// Allocates single value on stack. Returns reference to allocated value
    pub fn alloc_stack(&mut self, tp: &CircomValueType) -> CircomStackValueRef {
        let vals = self.alloc_stack_n(&vec![tp.clone()]);
        assert!(vals.len() == 1);
        return vals[0].clone();
    }

    /// Loads reference to another stack value on stack
    pub fn load_stack_ref(&mut self, val_ref: &CircomStackValueRef) {
        let val = val_ref.value_rc.borrow();
        assert!(!val.deallocated);

        match &val.kind {
            CircomStackValueKind::MemoryStackValue(tp, mem_val) => {
                self.push(CircomStackValueKind::MemoryStackValuePtr(tp.clone(), mem_val.clone()));
            }
            _ => {
                panic!("don't know how to load reference to stack value");
            }
        }
    }

    /// Loads reference to subarray of array stack value
    pub fn load_stack_array_ref(&mut self,
                                val_ref: &CircomStackValueRef,
                                offset: usize,
                                size: usize) {
        let val = val_ref.value_rc.borrow();
        assert!(!val.deallocated);

        match &val.kind {
            CircomStackValueKind::MemoryStackValue(tp, mem_val) => {
                match tp {
                    CircomValueType::FRArray(size) => {
                        // checking offset and size
                        if offset + size > *size {
                            panic!("Invalid stack subarray offset and size");
                        }
                        self.push(CircomStackValueKind::MemoryStackValueArrayPtr(
                                  tp.clone(), mem_val.clone(), offset, *size));
                    }
                    _ => {
                        panic!("can't load subarray of non array value");
                    }
                }
            }
            _ => {
                panic!("don't know how to load subarray of stack value");
            }
        }
    }

    /// Loads reference to element of array stack value
    pub fn load_stack_array_element_ref(&mut self,
                                        val_ref: &CircomStackValueRef,
                                        offset: usize) {
        let val = val_ref.value_rc.borrow();
        assert!(!val.deallocated);

        match &val.kind {
            CircomStackValueKind::MemoryStackValue(tp, mem_val) => {
                match tp {
                    CircomValueType::FRArray(size) => {
                        // checking offset
                        if offset >= *size {
                            panic!("Invalid array element offset");
                        }
                        self.push(CircomStackValueKind::MemoryStackValueArrayElementPtr(
                                  tp.clone(), mem_val.clone(), offset));
                    }
                    _ => {
                        panic!("can't load element of non array value");
                    }
                }
            }
            _ => {
                panic!("don't know how to load array element of stack value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Common stack values functions

    /// Returns stack value kind
    pub fn value_kind(&self, val_ref: &CircomStackValueRef) -> CircomStackValueKind {
        let val = val_ref.value_rc.borrow();
        if val.deallocated {
            panic!("Access to deallocated stack value");
        }

        return val.kind.clone();
    }

    /// Returns stack value type
    pub fn value_type(&self, val_ref: &CircomStackValueRef) -> CircomValueType {
        return match self.value_kind(val_ref) {
            CircomStackValueKind::WASMConst(tp, _) => CircomValueType::WASM(tp),
            CircomStackValueKind::MemoryPtrConst(tp, _) => tp,
            CircomStackValueKind::LocalVariablePtr(var) => { self.local_var_type(&var).clone() },
            CircomStackValueKind::LocalVariableArrayPtr(_var, _offset, size) => {
                CircomValueType::FRArray(size)
            },
            CircomStackValueKind::LocalVariableArrayElementPtr(_var, _offset) => {
                CircomValueType::FR
            },
            CircomStackValueKind::ParameterPtr(par) => { self.param_type(&par).clone() },
            CircomStackValueKind::ParameterArrayPtr(_par, _offset, size) => {
                CircomValueType::FRArray(size)
            },
            CircomStackValueKind::ParameterArrayElementPtr(_par, _offset) => CircomValueType::FR,
            CircomStackValueKind::ReturnValuePtr(ret) => { self.ret_val_type(&ret).clone() },
            CircomStackValueKind::ReturnValueArrayPtr(_ret, _offset, size) => {
                CircomValueType::FRArray(size)
            },
            CircomStackValueKind::ReturnValueArrayElementPtr(_ret, _offset) => {
                CircomValueType::FR
            },
            CircomStackValueKind::MemoryPtrWASMLocal(tp, _) => tp,
            CircomStackValueKind::MemoryStackValue(tp, _) => tp,
            CircomStackValueKind::MemoryStackValuePtr(tp, _) => tp,
            CircomStackValueKind::MemoryStackValueArrayPtr(_tp, _val, _offset, size) => {
                CircomValueType::FRArray(size)
            },
            CircomStackValueKind::MemoryStackValueArrayElementPtr(_tp, _val, _offset) => {
                CircomValueType::FR
            },
            CircomStackValueKind::WASMLocal(wasm_loc) => {
                let (_, wasm_tp) = self.func.borrow_mut().local(&wasm_loc);
                CircomValueType::WASM(wasm_tp)
            },
            CircomStackValueKind::WASMSTack(wasm_tp) => CircomValueType::WASM(wasm_tp)
        };
    }

    /// Loads WASM const value on stack
    pub fn load_const(&mut self, tp: WASMType, val: i64) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::WASMConst(tp, val));
    }

    /// Pushes value to stack
    fn push(&mut self, kind: CircomStackValueKind) -> CircomStackValueRef {
        let value = CircomStackValue::new(kind);
        let value_rc = Rc::new(RefCell::new(value));
        self.values.push(value_rc.clone());
        return CircomStackValueRef::new(value_rc);
    }

    /// Pushes value stored in global memory at constant address
    pub fn push_mem_const(&mut self,
                          tp: CircomValueType,
                          addr: usize) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::MemoryPtrConst(tp, addr));
    }

    /// Pushes value stored in WASM local
    pub fn push_wasm_local(&mut self, loc: &WASMLocalVariableRef) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::WASMLocal(loc.clone()));
    }

    /// Pushes value stored in WASM stack
    pub fn push_wasm_stack(&mut self, tp: WASMType) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::WASMSTack(tp));
    }

    /// Pushes pointer to memory stack value
    pub fn push_mem_stack_ptr(&mut self,
                              tp: CircomValueType,
                              val_ref: &MemoryStackValueRef) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::MemoryStackValuePtr(tp, val_ref.clone()));
    }

    /// Allocates new circom values on memory stack
    pub fn alloc_mem_stack(&mut self, types: &Vec<CircomValueType>) -> Vec<MemoryStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        for (i, tp) in types.iter().enumerate() {
            let kind = CircomStackValueKind::MemoryStackValue(tp.clone(), stack_vals[i].clone());
            self.push(kind);
        }

        return stack_vals;
    }

    /// Pops single WASM value of specified type from stack
    fn pop_wasm_stack_impl(&mut self, opt_type: Option<WASMType>) {
        match self.values.last().expect("expected WASM value, but stack is empty").borrow().kind {
            CircomStackValueKind::WASMSTack(stack_type) => {
                match opt_type {
                    Some(t) => {
                        if stack_type != t {
                            panic!("expected WASM value of type {}, found {}",
                                   t.to_string(),
                                   stack_type.to_string());
                        }
                    }
                    None => {}
                }
            }
            _ => {
                panic!("expected WASM value on stack")
            }
        }

        self.pop(1);
    }

    /// Pops single WASM value of specified type from stack
    pub fn pop_wasm_stack(&mut self, opt_type: &WASMType) {
        self.pop_wasm_stack_impl(Some(*opt_type));
    }

    /// Pops single WASM value of any type from stack
    pub fn pop_wasm_stack_any(&mut self) {
        self.pop_wasm_stack_impl(None);
    }

    /// Checks that values located on top of stack is suitable for WASM pointer arithmetics
    pub fn pop_wasm_ptr_ops(&mut self) {
        if self.values.len() < 2 {
            panic!("Expected stack values for pointer arithmetics, but stack size is less than 2");
        }

        let t1 = match self.values.last().expect("expected WASM value, but stack is empty")
                           .borrow().kind {
            CircomStackValueKind::WASMSTack(tp) => tp,
            _ => {
                panic!("expected WASM value on stack")
            }
        };
        self.pop(1);

        let t2 = match self.values.last().expect("expected WASM value, but stack is empty")
                           .borrow().kind {
            CircomStackValueKind::WASMSTack(tp) => tp,
            _ => {
                panic!("expected WASM value on stack")
            }
        };
        self.pop(1);

        if t1 == WASMType::I32 && t2 == WASMType::PTR ||
           t1 == WASMType::PTR && t2 == WASMType::I32 {
            return;
        }

        panic!("Expected stack values for pointer arithmetics, but found: {}, {}",
               t1.to_string(), t2.to_string());
    }

    /// Pops values from stack
    pub fn pop(&mut self, count: usize) {
        let mut mem_stack_drop_count: usize = 0;

        for _ in 0 .. count {
            {
                let val = self.values.last().unwrap().borrow();
                match &val.kind {
                    CircomStackValueKind::WASMConst(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryPtrConst(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryPtrWASMLocal(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariablePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariableArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariableArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValuePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValueArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValueArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueKind::MemoryStackValuePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValueArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValueArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::WASMLocal(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::WASMSTack(..) => {
                        // doing nothing
                    }
                }
            }

            // checking for branching
            match self.branch_starts.last() {
                Some(idx) => {
                    if *idx == self.values.len() {
                        panic!("can't finish branch: additional values present on stack");
                    }
                }
                None => {}
            }

            self.values.pop();
        }

        // deallocating memory stack values
        self.mem_frame.borrow_mut().pop(mem_stack_drop_count);
    }

    /// Returns reference to value located at n-th place on top of stack
    pub fn top(&self, n: usize) -> CircomStackValueRef {
        if self.values.len() <= n {
            panic!("Can't get value on {}-th place on top of stack", n);
        }

        return CircomStackValueRef::new(self.values[self.values.len() - n - 1].clone());
    }

    /// Generates loading Circom value on top of WASM stack for passing as function parameter.
    /// Returns WASM type loaded on stack
    pub fn gen_wasm_stack_load_no_push(&mut self, val: CircomStackValueRef) -> WASMType {
        match self.value_kind(&val) {
            CircomStackValueKind::WASMConst(wasm_type, val) => {
                self.func.borrow_mut().gen_const(wasm_type, val);
                return wasm_type;
            }
            CircomStackValueKind::MemoryPtrConst(_, addr) => {
                self.func.borrow_mut().gen_const(WASMType::PTR, addr as i64);
                return WASMType::PTR;
            },
            CircomStackValueKind::MemoryPtrWASMLocal(_, wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
                return WASMType::PTR;
            },
            CircomStackValueKind::LocalVariablePtr(var) => {
                let mem_loc = self.local_var_mem_loc(&var);
                self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                return WASMType::PTR;
            },
            CircomStackValueKind::LocalVariableArrayPtr(var, offset, _) => {
                let mem_loc = self.local_var_mem_loc(&var);
                self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::LocalVariableArrayElementPtr(var, offset) => {
                let mem_loc = self.local_var_mem_loc(&var);
                self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::ParameterPtr(par) => {
                let ptr_wasm_loc = self.param_wasm_loc(&par);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                return WASMType::PTR;
            },
            CircomStackValueKind::ParameterArrayPtr(par, offset, _) => {
                let ptr_wasm_loc = self.param_wasm_loc(&par);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::ParameterArrayElementPtr(par, offset) => {
                let ptr_wasm_loc = self.param_wasm_loc(&par);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::ReturnValueArrayPtr(ret_val, offset, _) => {
                let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::ReturnValueArrayElementPtr(ret_val, offset) => {
                let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                self.func.borrow_mut().gen_add(WASMType::PTR);
                return WASMType::PTR;
            },
            CircomStackValueKind::ReturnValuePtr(ret_val) => {
                let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                return WASMType::PTR;
            },
            CircomStackValueKind::MemoryStackValue(_, stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val, 0);
                return WASMType::PTR;
            },
            CircomStackValueKind::MemoryStackValuePtr(_, stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val, 0);
                return WASMType::PTR;
            },
            CircomStackValueKind::MemoryStackValueArrayPtr(_tp, stack_val, offset, _size) => {
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val, byte_offset);
                return WASMType::PTR;
            },
            CircomStackValueKind::MemoryStackValueArrayElementPtr(_tp, stack_val, offset) => {
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val, byte_offset);
                return WASMType::PTR;
            },
            CircomStackValueKind::WASMLocal(wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
                let (_, tp) = self.func.borrow().local(&wasm_loc);
                return tp;
            },
            CircomStackValueKind::WASMSTack(..) => {
                panic!("Duplicating of WASM stack values is not supported");
            }
        }
    }

    /// Generates loading Circom value on top of WASM stack for passing as function parameter.
    /// Pushes loaded WASM type on logical stack.
    pub fn gen_wasm_stack_load(&mut self, val: CircomStackValueRef) {
        let tp = self.gen_wasm_stack_load_no_push(val);
        self.push(CircomStackValueKind::WASMSTack(tp));
    }

    /// Drops specified number of values located on top of stack
    pub fn drop(&mut self, count: usize) {
        // removing values from vector of values and calculating number of required operations
        let mut wasm_stack_drop_count: usize = 0;
        let mut mem_stack_drop_count: usize = 0;
        for _ in 0 .. count {
            {
                let val = self.values.last().unwrap().borrow();
                match &val.kind {
                    CircomStackValueKind::WASMConst(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::MemoryPtrConst(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::MemoryPtrWASMLocal(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariablePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariableArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::LocalVariableArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ParameterArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValuePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValueArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValueArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueKind::MemoryStackValuePtr(..) => {
                        // doing nothing for references to memory stack
                    },
                    CircomStackValueKind::MemoryStackValueArrayPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValueArrayElementPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::WASMLocal(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::WASMSTack(..) => {
                        // dropping value from top of stack
                        wasm_stack_drop_count += 1;
                    }
                }
            }

            // checking for branching
            match self.branch_starts.last() {
                Some(idx) => {
                    if *idx == self.values.len() {
                        panic!("can't finish branch: additional values present on stack");
                    }
                }
                None => {}
            }

            self.values.pop();
        }

        // dropping wasm stack values
        for _ in 0 .. wasm_stack_drop_count {
            self.func.borrow_mut().gen_drop();
        }

        // deallocating memory stack values
        self.mem_frame.borrow_mut().pop(mem_stack_drop_count);
    }

    /// Checks that stack is empty
    pub fn check_empty(&self) {
        if !self.values.is_empty() {
            println!("Stack is not empty at the end of function {}:\n{}\n",
                     self.func.borrow().name(),
                     self.dump());
        }

        self.mem_frame.borrow().check_empty();
    }

    /// Dumps stack contents to string
    pub fn dump(&self) -> String {
        return self.values.iter()
            .enumerate()
            .map(|(idx, val)| {
                let kind_str = match &val.borrow().kind {
                    CircomStackValueKind::WASMConst(tp, val) => {
                        format!("WASM CONST {} {}", tp.to_string(), val)
                    }
                    CircomStackValueKind::MemoryPtrConst(tp, addr) => {
                        format!("MEM PTR CONST {} {}", tp.to_string(), addr)
                    }
                    CircomStackValueKind::MemoryPtrWASMLocal(tp, loc) => {
                        format!("MEM PTR WASMLOC {} {}",
                                tp.to_string(),
                                self.func.borrow().gen_local_ref(loc))
                    }
                    CircomStackValueKind::LocalVariablePtr(var) => {
                        let var_loc = self.local_var_mem_loc(var);
                        let loc_str = self.mem_frame.borrow().dump_local(var_loc);
                        format!("LOCAL {} {} {}",
                                self.local_var_name(var),
                                self.local_var_type(var).to_string(),
                                loc_str)
                    }
                    CircomStackValueKind::LocalVariableArrayPtr(var, offset, size) => {
                        let var_loc = self.local_var_mem_loc(var);
                        let loc_str = self.mem_frame.borrow().dump_local(var_loc);
                        format!("LOCAL {}[{}, {}) {} {}",
                                self.local_var_name(var),
                                offset,
                                offset + size,
                                self.local_var_type(var).to_string(),
                                loc_str)
                    }
                    CircomStackValueKind::LocalVariableArrayElementPtr(var, offset) => {
                        let var_loc = self.local_var_mem_loc(var);
                        let loc_str = self.mem_frame.borrow().dump_local(var_loc);
                        format!("LOCAL {}[{}] {} {}",
                                self.local_var_name(var),
                                offset,
                                self.local_var_type(var).to_string(),
                                loc_str)
                    }
                    CircomStackValueKind::ParameterPtr(par) => {
                        let loc = self.param_wasm_loc(par);
                        let name = self.param_name(par);
                        let type_str = self.param_type(par).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("PARAM {} {} {}", name, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ParameterArrayPtr(par, offset, size) => {
                        let loc = self.param_wasm_loc(par);
                        let name = self.param_name(par);
                        let type_str = self.param_type(par).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("PARAM {}[{}, {}) {} {}",
                                name, offset, offset + size, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ParameterArrayElementPtr(par, offset) => {
                        let loc = self.param_wasm_loc(par);
                        let name = self.param_name(par);
                        let type_str = self.param_type(par).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("PARAM {}[{}) {} {}",
                                name, offset, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ReturnValuePtr(ret_val) => {
                        let loc = self.ret_val_wasm_loc(ret_val);
                        let name = self.ret_val_idx(ret_val).to_string();
                        let type_str = self.ret_val_type(ret_val).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("RETVAL {} {} {}", name, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ReturnValueArrayPtr(ret_val, offset, size) => {
                        let loc = self.ret_val_wasm_loc(ret_val);
                        let name = self.ret_val_idx(ret_val).to_string();
                        let type_str = self.ret_val_type(ret_val).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("RETVAL {}[{}, {}) {} {}",
                                name, offset, offset + size, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ReturnValueArrayElementPtr(ret_val, offset) => {
                        let loc = self.ret_val_wasm_loc(ret_val);
                        let name = self.ret_val_idx(ret_val).to_string();
                        let type_str = self.ret_val_type(ret_val).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("RETVAL {}[{}] {} {}",
                                name, offset, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::MemoryStackValue(tp, val) => {
                        format!("MEM STACK {} {}", tp.to_string(), val.dump(true))
                    }
                    CircomStackValueKind::MemoryStackValuePtr(tp, val) => {
                        format!("MEM STACK PTR {} {}", tp.to_string(), val.dump(true))
                    }
                    CircomStackValueKind::MemoryStackValueArrayPtr(tp, val, offset, size) => {
                        let name = val.dump(true);
                        let type_str = tp.to_string();

                        format!("MEM STACK PTR {}[{}, {}) {}",
                                name, offset, offset + size, type_str)
                    }
                    CircomStackValueKind::MemoryStackValueArrayElementPtr(tp, val, offset) => {
                        let name = val.dump(true);
                        let type_str = tp.to_string();

                        format!("MEM STACK PTR {}[{}] {}", name, offset, type_str)
                    }
                    CircomStackValueKind::WASMLocal(loc) => {
                        format!("WASM LOCAL {}", self.func.borrow().gen_local_ref(loc))
                    }
                    CircomStackValueKind::WASMSTack(tp) => {
                        format!("WASM STACK {}", tp.to_string())
                    }
                };

                return format!("{:3}\t{}", idx, kind_str);
            })
            .collect::<Vec<String>>()
            .join("\n");
    }

    /// Starts new branch in stack
    pub fn start_branch(&mut self) {
        // starting branch in memory stack
        self.mem_frame.borrow_mut().start_branch();

        self.branch_starts.push(self.values.len());
    }

    /// Checks that current stack state is equal to state before beginning
    /// of current branch
    pub fn check_branch(&self) {
        self.mem_frame.borrow().check_branch();

        match self.branch_starts.last() {
            Some(idx) => {
                if *idx != self.values.len() {
                    panic!("additional values present on stack for current branch:\n{}",
                           self.dump());
                }
            }
            None => {
                panic!("no started branch found");
            }
        }
    }

    /// Finishes branch in stack. Verifies that stack state after branching
    /// is equal to stack state before branching
    pub fn end_branch(&mut self) {
        self.check_branch();
        let res = self.branch_starts.pop();
        assert!(res.is_some());
    }

    /// Generates WASM local.get instruction and pushes WASM value on logical stack
    pub fn gen_wasm_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        let (_, tp) = self.func.borrow().local(var_ref);

        self.func.borrow_mut().gen_local_get(var_ref);
        self.push_wasm_stack(tp);
    }
}
