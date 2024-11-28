
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

    /// Pointer to parameter
    ParameterPtr(CircomParameterRef),

    /// Pointer to return value
    ReturnValuePtr(CircomReturnValueRef),

    /// Value stored in memory stack
    MemoryStackValue(CircomValueType, MemoryStackValueRef),

    /// Pointer to value stored in memory stack
    MemoryStackValuePtr(CircomValueType, MemoryStackValueRef),

    /// Value stored in local WASM variable
    WASMLocal(WASMLocalVariableRef),

    /// Value stored in WASM stack
    WASMSTack(WASMType),
}

impl CircomStackValueKind {
    /// Returns true if location is a pointer
    pub fn is_ptr(&self) -> bool {
        match self {
            CircomStackValueKind::WASMConst(..) => false,
            CircomStackValueKind::MemoryPtrConst(..) => true,
            CircomStackValueKind::MemoryPtrWASMLocal(..) => true,
            CircomStackValueKind::LocalVariablePtr(..) => true,
            CircomStackValueKind::ParameterPtr(..) => true,
            CircomStackValueKind::ReturnValuePtr(..) => true,
            CircomStackValueKind::MemoryStackValue(..) => false,
            CircomStackValueKind::MemoryStackValuePtr(..) => true,
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
        self.push(CircomStackValueKind::LocalVariablePtr(var_ref.clone()));
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
    pub fn load_param_ref(&mut self, par_ref: &CircomParameterRef) {
        self.push(CircomStackValueKind::ParameterPtr(par_ref.clone()));
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


    ////////////////////////////////////////////////////////////
    /// Stack values

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
            CircomStackValueKind::ParameterPtr(par) => { self.param_type(&par).clone() },
            CircomStackValueKind::ReturnValuePtr(ret) => { self.ret_val_type(&ret).clone() },
            CircomStackValueKind::MemoryPtrWASMLocal(tp, _) => tp,
            CircomStackValueKind::MemoryStackValue(tp, _) => tp,
            CircomStackValueKind::MemoryStackValuePtr(tp, _) => tp,
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
    pub fn alloc_mem_stack(&mut self, types: Vec<CircomValueType>) -> Vec<MemoryStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        for (i, tp) in types.iter().enumerate() {
            let kind = CircomStackValueKind::MemoryStackValue(tp.clone(), stack_vals[i].clone());
            self.push(kind);
        }

        return stack_vals;
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
                    CircomStackValueKind::ParameterPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValuePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueKind::MemoryStackValuePtr(..) => {
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
        match self.value_kind(&val) {
            CircomStackValueKind::WASMConst(wasm_type, val) => {
                self.func.borrow_mut().gen_const(wasm_type, val);
            }
            CircomStackValueKind::MemoryPtrConst(_, addr) => {
                self.func.borrow_mut().gen_const(WASMType::PTR, addr as i64);
            },
            CircomStackValueKind::MemoryPtrWASMLocal(_, wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
            },
            CircomStackValueKind::LocalVariablePtr(var) => {
                let mem_loc = self.local_var_mem_loc(&var);
                self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
            },
            CircomStackValueKind::ParameterPtr(par) => {
                let ptr_wasm_loc = self.param_wasm_loc(&par);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
            },
            CircomStackValueKind::ReturnValuePtr(ret_val) => {
                let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
            }
            CircomStackValueKind::MemoryStackValue(_, stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val);
            },
            CircomStackValueKind::MemoryStackValuePtr(_, stack_val) => {
                self.mem_frame.borrow_mut().gen_load_value_addr(&stack_val);
            },
            CircomStackValueKind::WASMLocal(wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
            },
            CircomStackValueKind::WASMSTack(..) => {
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
                    CircomStackValueKind::ParameterPtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ReturnValuePtr(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::MemoryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueKind::MemoryStackValuePtr(..) => {
                        // doing nothing for references to memory stack
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
                    CircomStackValueKind::ParameterPtr(par) => {
                        let loc = self.param_wasm_loc(par);
                        let name = self.param_name(par);
                        let type_str = self.param_type(par).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("PARAM {} {} {}", name, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::ReturnValuePtr(ret_val) => {
                        let loc = self.ret_val_wasm_loc(ret_val);
                        let name = self.ret_val_idx(ret_val).to_string();
                        let type_str = self.ret_val_type(ret_val).to_string();
                        let wasm_loc_str = self.func.borrow().gen_local_ref(loc);

                        format!("RETVAL {} {} {}", name, type_str, wasm_loc_str)
                    }
                    CircomStackValueKind::MemoryStackValue(tp, val) => {
                        format!("MEM STACK {} {}", tp.to_string(), val.dump(true))
                    }
                    CircomStackValueKind::MemoryStackValuePtr(tp, val) => {
                        format!("MEM STACK PTR {} {}", tp.to_string(), val.dump(true))
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
}
