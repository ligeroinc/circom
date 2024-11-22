
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
pub enum CircomStackValueLocation {
    /// Memory pointer referenced by local WASM variable
    MemoryPtrWASMLocal(WASMLocalVariableRef),

    /// Pointer to local variable in memory stack frame
    MemoryStackLocalPtr(MemoryStackLocalRef),

    /// Value stored in memory stack
    MemoryStackValue(MemoryStackValueRef),

    /// Pointer to value stored in memory stack
    MemoryStackValuePtr(MemoryStackValueRef),

    /// Value stored in local WASM variable
    WASMLocal(WASMLocalVariableRef),

    /// Value stored in WASM stack
    WASMSTack
}

impl CircomStackValueLocation {
    /// Returns true if location is a pointer
    pub fn is_ptr(&self) -> bool {
        match self {
            CircomStackValueLocation::MemoryPtrWASMLocal(_) => true,
            CircomStackValueLocation::MemoryStackLocalPtr(_) => true,
            CircomStackValueLocation::MemoryStackValue(_) => false,
            CircomStackValueLocation::MemoryStackValuePtr(_) => true,
            CircomStackValueLocation::WASMLocal(_) => false,
            CircomStackValueLocation::WASMSTack => false
        }
    }
}


/// Value located in Circom stack frame
#[derive(Clone)]
pub struct CircomStackValue {
    /// Type of value
    type_: CircomValueType,

    /// Value location
    loc: CircomStackValueLocation,

    /// Was this value deallocated
    deallocated: bool
}

impl CircomStackValue {
    /// Creates new stack value
    fn new(tp: CircomValueType, loc: CircomStackValueLocation) -> CircomStackValue {
        return CircomStackValue {
            type_: tp,
            loc: loc,
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
    values: Vec<Rc<RefCell<CircomStackValue>>>
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
            values: vec![]
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
        let var = &self.locals[var_ref.idx];
        self.push_mem_local(var.type_.clone(), var.loc.clone());
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
        return &self.locals[par.idx].type_;
    }

    /// Returns reference to WASM local containing address of parameter
    pub fn param_wasm_loc(&self, par: &CircomParameterRef) -> &WASMLocalVariableRef {
        return &self.params[par.idx].wasm_loc;
    }

    /// Loads reference to function parameter to stack
    pub fn load_param_ref(&mut self, par_ref: &CircomParameterRef) {
        let par = &self.params[par_ref.idx];
        self.push_mem(par.type_.clone(), par.wasm_loc.clone());
    }


    ////////////////////////////////////////////////////////////
    /// Return values

    /// Creates new return value in stack frame
    pub fn new_ret_val(&mut self, tp: CircomValueType) -> CircomReturnValueRef {
        let ret_val_idx = self.ret_vals.len();

        // creating WASM parameter for pointer to return value memory
        let name = format!("ret_val_{}", ret_val_idx);
        let wasm_par = self.func.borrow_mut().new_param(&name, WASMType::PTR);

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
        let par = &self.ret_vals[ret_val_ref.idx];
        self.push_mem(par.type_.clone(), par.wasm_loc.clone());
    }


    ////////////////////////////////////////////////////////////
    /// Stack values

    /// Returns type and location of stack value
    pub fn value(&self, val_ref: &CircomStackValueRef) ->
            (CircomValueType, CircomStackValueLocation) {
        let val = val_ref.value_rc.borrow();
        if val.deallocated {
            panic!("Access to deallocated stack value");
        }

        return (val.type_.clone(), val.loc.clone());
    }

    /// Pushes value to stack
    fn push(&mut self, tp: CircomValueType, loc: CircomStackValueLocation) -> CircomStackValueRef {
        let value = CircomStackValue::new(tp, loc);
        let value_rc = Rc::new(RefCell::new(value));
        self.values.push(value_rc.clone());
        return CircomStackValueRef::new(value_rc);
    }

    /// Pushes value stored in global memory referenced by pointer in WASM local variable
    pub fn push_mem(&mut self,
                    tp: CircomValueType,
                    wasm_loc: WASMLocalVariableRef) -> CircomStackValueRef {
        return self.push(tp, CircomStackValueLocation::MemoryPtrWASMLocal(wasm_loc));
    }

    /// Pushes value stored in memory stack local
    pub fn push_mem_local(&mut self,
                          tp: CircomValueType,
                          mem_loc: MemoryStackLocalRef) -> CircomStackValueRef {
        return self.push(tp, CircomStackValueLocation::MemoryStackLocalPtr(mem_loc));
    }

    /// Pushes value stored in WASM local
    pub fn push_wasm_local(&mut self, loc: &WASMLocalVariableRef) -> CircomStackValueRef {
        let (_, wasm_type) = self.func.borrow().local(loc);
        let value_type = CircomValueType::WASM(wasm_type);
        return self.push(value_type, CircomStackValueLocation::WASMLocal(loc.clone()));
    }

    /// Pushes value stored in WASM stack
    pub fn push_wasm_stack(&mut self, tp: CircomValueType) -> CircomStackValueRef {
        return self.push(tp, CircomStackValueLocation::WASMSTack);
    }

    /// Pushes pointer to memory stack value
    pub fn push_mem_stack_ptr(&mut self,
                              tp: CircomValueType,
                              val_ref: &MemoryStackValueRef) -> CircomStackValueRef {
        return self.push(tp, CircomStackValueLocation::MemoryStackValuePtr(val_ref.clone()));
    }

    /// Allocates new circom values on memory stack
    pub fn alloc_mem_stack(&mut self, types: Vec<CircomValueType>) -> Vec<MemoryStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        for (i, tp) in types.iter().enumerate() {
            let loc = CircomStackValueLocation::MemoryStackValue(stack_vals[i].clone());
            self.push(tp.clone(), loc);
        }

        return stack_vals;
    }

    /// Pops values from stack
    pub fn pop(&mut self, count: usize) {
        let mut mem_stack_drop_count: usize = 0;

        for _ in 0 .. count {
            {
                let val = self.values.last().unwrap().borrow();
                match &val.loc {
                    CircomStackValueLocation::MemoryPtrWASMLocal(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::MemoryStackLocalPtr(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::MemoryStackValue(_) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueLocation::MemoryStackValuePtr(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::WASMLocal(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::WASMSTack => {
                        // doing nothing
                    }
                }
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

    /// Generates loading Circom value on top of WASM stack for passing as function parameter
    pub fn gen_wasm_stack_load(&mut self, val: CircomStackValueRef) {
        let (_, loc) = self.value(&val);
        match loc {
            CircomStackValueLocation::MemoryPtrWASMLocal(wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
            },
            CircomStackValueLocation::MemoryStackLocalPtr(stack_loc) => {
                self.mem_frame.borrow_mut().gen_load_local_addr(&stack_loc);
            },
            CircomStackValueLocation::MemoryStackValue(stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val);
            },
            CircomStackValueLocation::MemoryStackValuePtr(stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val);
            },
            CircomStackValueLocation::WASMLocal(wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
            },
            CircomStackValueLocation::WASMSTack => {
                panic!("Duplicating of WASM stack values is not supported");
            }
        }
    }

    /// Drops specified number of values located on top of stack
    pub fn drop(&mut self, count: usize) {
        // removing values from vector of values and calculating number of required operations
        let mut wasm_stack_drop_count: usize = 0;
        let mut mem_stack_drop_count: usize = 0;
        for _ in 0 .. count {
            {
                let val = self.values.last().unwrap().borrow();
                match &val.loc {
                    CircomStackValueLocation::MemoryPtrWASMLocal(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::MemoryStackLocalPtr(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::MemoryStackValue(_) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueLocation::MemoryStackValuePtr(_) => {
                        // doing nothing for references to memory stack
                    },
                    CircomStackValueLocation::WASMLocal(_) => {
                        // doing nothing
                    },
                    CircomStackValueLocation::WASMSTack => {
                        // dropping value from top of stack
                        wasm_stack_drop_count += 1;
                    }
                }
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

    /// Loads reference to value on top of stack
    pub fn load_ref(&mut self, val: &CircomValueRef) {
        match val {
            CircomValueRef::MemoryRefWASMLocal(loc) => {
                self.push_mem(CircomValueType::FR, loc.clone());
            },
            CircomValueRef::MemoryStackLocal(loc) => {
                self.push_mem_local(CircomValueType::FR, loc.clone());
            }
        }
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
                let loc_str = match &val.borrow().loc {
                    CircomStackValueLocation::MemoryPtrWASMLocal(loc) => {
                        format!("MEM PTR WASMLOC {}", self.func.borrow().gen_local_ref(loc))
                    }
                    CircomStackValueLocation::MemoryStackLocalPtr(loc) => {
                        format!("MEM LOCAL {}", self.mem_frame.borrow().dump_local(loc))
                    }
                    CircomStackValueLocation::MemoryStackValue(val) => {
                        format!("MEM STACK {}", val.dump(true))
                    },
                    CircomStackValueLocation::MemoryStackValuePtr(val) => {
                        format!("MEM STACK PTR {}", val.dump(true))
                    }
                    CircomStackValueLocation::WASMLocal(loc) => {
                        format!("WASM LOCAL {}", self.func.borrow().gen_local_ref(loc))
                    }
                    CircomStackValueLocation::WASMSTack => {
                        format!("WASM STACK")
                    }
                };

                return format!("{:3}\t{}\t{}", idx, val.borrow().type_.to_string(), loc_str);
            })
            .collect::<Vec<String>>()
            .join("\n");
    }
}
