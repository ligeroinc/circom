
use crate::ligetron_elements::wasm;

use super::memory_stack;
use super::memory_stack::*;
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


/// Temporary stack value
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


#[derive(Clone)]
pub enum CircomValueRef {
    LocalVariable(CircomLocalVariableRef),
    Parameter(CircomParameterRef),
    ReturnValue(CircomReturnValueRef),
    TemporaryStackValue(TemporaryStackValueRef)
}


pub trait ConvertibleToValueRef {
    fn to_ref(&self) -> CircomValueRef;
}

impl ConvertibleToValueRef for CircomLocalVariableRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::LocalVariable(self.clone());
    }
}

impl ConvertibleToValueRef for CircomParameterRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::Parameter(self.clone());
    }
}

impl ConvertibleToValueRef for CircomReturnValueRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::ReturnValue(self.clone());
    }
}

impl ConvertibleToValueRef for TemporaryStackValueRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::TemporaryStackValue(self.clone());
    }
}


/// Location of value stored on Circom stack frame
pub enum CircomStackValueKind {
    /// WASM constant value
    WASMConst(WASMType, i64),

    /// Memory pointer with fixed value
    MemoryPtrConst(CircomValueType, usize),

    /// Temporary value stored in stack
    TemporaryStackValue(TemporaryStackValueRef),

    /// Reference to another value
    ValueRef(CircomValueRef),

    /// Reference to subarray of another array (ref, offset, size)
    ArraySliceRef(CircomValueRef, usize, usize),

    /// Reference to element of array (ref, offset)
    ArrayElementRef(CircomValueRef, usize),

    /// Value stored in local WASM variable
    WASMLocal(WASMLocalVariableRef),

    /// Value stored in WASM stack
    WASMSTack(WASMType),
}

impl CircomStackValueKind {
    /// Returns true if value is a reference to another value
    pub fn is_ref(&self) -> bool {
        match self {
            CircomStackValueKind::WASMConst(..) => false,
            CircomStackValueKind::MemoryPtrConst(..) => true,
            CircomStackValueKind::TemporaryStackValue(..) => false,
            CircomStackValueKind::ValueRef(..) => true,
            CircomStackValueKind::ArraySliceRef(..) => true,
            CircomStackValueKind::ArrayElementRef(..) => true,
            CircomStackValueKind::WASMLocal(..) => false,
            CircomStackValueKind::WASMSTack(..) => false
        }
    }
}


/// Value located in Circom stack frame
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

    /// Returns true if value is located on WASM stack
    pub fn is_wasm_stack(&self) -> bool {
        match self.value_rc.borrow().kind {
            CircomStackValueKind::WASMSTack(..) => true,
            _ => false
        }
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
        let val_ref = CircomValueRef::LocalVariable(var_ref.clone());
        self.push(CircomStackValueKind::ValueRef(val_ref));
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

                let val_ref = CircomValueRef::LocalVariable(var.clone());
                self.push(CircomStackValueKind::ArraySliceRef(val_ref, offset, size));
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

                let val_ref = CircomValueRef::LocalVariable(arr.clone());
                self.push(CircomStackValueKind::ArrayElementRef(val_ref, offset));
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


    ////////////////////////////////////////////////////////////
    /// Temporary stack values

    /// Allocates multiple temporary values on stack. Returns references to allocated value
    pub fn alloc_temp_n(&mut self, types: &Vec<CircomValueType>) -> Vec<TemporaryStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let mem_stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        let mut vals = Vec::<TemporaryStackValueRef>::new();
        for (idx, mem_val) in mem_stack_vals.iter().enumerate() {
            let temp_val = TemporaryStackValueRef::new(types[idx].clone(), mem_val.clone());
            self.push(CircomStackValueKind::TemporaryStackValue(temp_val.clone()));
            vals.push(temp_val);
        }

        return vals;
    }

    /// Allocates single temporary value on stack. Returns reference to allocated value
    pub fn alloc_temp(&mut self, tp: &CircomValueType) -> TemporaryStackValueRef {
        let vals = self.alloc_temp_n(&vec![tp.clone()]);
        assert!(vals.len() == 1);
        return vals[0].clone();
    }


    ////////////////////////////////////////////////////////////
    /// References

    /// Returns referenced value type
    fn value_ref_type<'a, 'b: 'a>(&'a self, val_ref: &'b CircomValueRef) -> &'a CircomValueType {
        match val_ref {
            CircomValueRef::LocalVariable(var) => self.local_var_type(&var),
            CircomValueRef::Parameter(par) => self.param_type(&par),
            CircomValueRef::ReturnValue(ret_val) => self.ret_val_type(&ret_val),
            CircomValueRef::TemporaryStackValue(temp_val) => temp_val.tp()
        }
    }

    /// Loads reference to value on stack
    pub fn load_ref<T: ConvertibleToValueRef>(&mut self, val: &T) {
        self.push(CircomStackValueKind::ValueRef(val.to_ref()));
    }

    /// Loads reference to subarray of array to stack
    pub fn load_array_slice_ref<T: ConvertibleToValueRef>(&mut self,
                                                          arr: &T,
                                                          offset: usize,
                                                          size: usize) {
        let arr_ref = arr.to_ref();

        // checking parameter type
        match self.value_ref_type(&arr_ref) {
            CircomValueType::FRArray(sz) => {
                // checking offset and size
                if offset + size > *sz {
                    panic!("Invalid local var subarray offset and size");
                }

                self.push(CircomStackValueKind::ArraySliceRef(arr_ref, offset, size));
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }
    }

    /// Loads reference to element of array to stack
    pub fn load_array_element_ref<T: ConvertibleToValueRef>(&mut self, arr: &T, offset: usize) {
        let arr_ref = arr.to_ref();

        // checking array variable type
        match self.value_ref_type(&arr_ref) {
            CircomValueType::FRArray(sz) => {
                // checking offset
                if offset  >= *sz {
                    panic!("Invalid local var array element offset");
                }

                self.push(CircomStackValueKind::ArrayElementRef(arr_ref, offset));
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Common stack values functions

    /// Returns stack value type
    pub fn value_type(&self, val_ref: &CircomStackValueRef) -> CircomValueType {
        let val_kind = &val_ref.value_rc.borrow().kind;
        return match val_kind {
            CircomStackValueKind::WASMConst(tp, _) => CircomValueType::WASM(*tp),
            CircomStackValueKind::MemoryPtrConst(tp, _) => tp.clone(),
            CircomStackValueKind::TemporaryStackValue(temp_val) => temp_val.tp().clone(),
            CircomStackValueKind::ValueRef(val_ref) => self.value_ref_type(val_ref).clone(),
            CircomStackValueKind::ArraySliceRef(_arr, _offset, size) => {
                CircomValueType::FRArray(*size)
            },
            CircomStackValueKind::ArrayElementRef(_arr, _offset) => {
                CircomValueType::FR
            },
            CircomStackValueKind::WASMLocal(wasm_loc) => {
                let (_, wasm_tp) = self.func.borrow_mut().local(&wasm_loc);
                CircomValueType::WASM(wasm_tp)
            },
            CircomStackValueKind::WASMSTack(wasm_tp) => CircomValueType::WASM(*wasm_tp)
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

    /// Allocates new circom values on memory stack
    pub fn alloc_mem_stack(&mut self, types: &Vec<CircomValueType>) -> Vec<MemoryStackValueRef> {
        let sizes = types.iter().map(|t| t.size(self.size_32_bit)).collect();
        let stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        for (i, tp) in types.iter().enumerate() {
            let temp_val = TemporaryStackValueRef::new(tp.clone(), stack_vals[i].clone());
            let kind = CircomStackValueKind::TemporaryStackValue(temp_val);
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
                    CircomStackValueKind::TemporaryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    },
                    CircomStackValueKind::ValueRef(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::ArraySliceRef(..) => {
                        // doing nothing
                    },
                    CircomStackValueKind::ArrayElementRef(..) => {
                        // doing nothing
                    }
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
        let val_kind = &val.value_rc.borrow().kind;
        match val_kind {
            CircomStackValueKind::WASMConst(wasm_type, val) => {
                self.func.borrow_mut().gen_const(*wasm_type, *val);
                return *wasm_type;
            },
            CircomStackValueKind::MemoryPtrConst(_, addr) => {
                self.func.borrow_mut().gen_const(WASMType::PTR, *addr as i64);
                return WASMType::PTR;
            },
            CircomStackValueKind::TemporaryStackValue(temp_val) => {
                let mem_val = temp_val.mem_stack_val();
                self.mem_frame.borrow_mut().gen_load_value_addr(mem_val, 0);
                return WASMType::PTR;
            },
            CircomStackValueKind::WASMLocal(wasm_loc) => {
                self.func.borrow_mut().gen_local_get(&wasm_loc);
                let (_, tp) = self.func.borrow().local(&wasm_loc);
                return tp;
            },
            CircomStackValueKind::ValueRef(val_ref) => {
                match val_ref {
                    CircomValueRef::LocalVariable(var) => {
                        let mem_loc = self.local_var_mem_loc(&var);
                        self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                    }
                    CircomValueRef::Parameter(par) => {
                        let ptr_wasm_loc = self.param_wasm_loc(&par);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                    }
                    CircomValueRef::ReturnValue(ret_val) => {
                        let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                    }
                    CircomValueRef::TemporaryStackValue(temp_val) => {
                        let mem_val = temp_val.mem_stack_val();
                        self.mem_frame.borrow_mut().gen_load_value_addr(mem_val, 0);
                    }
                }

                return WASMType::PTR;
            },
            CircomStackValueKind::ArraySliceRef(arr, offset, _size) => {
                let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);

                match arr {
                    CircomValueRef::LocalVariable(var) => {
                        let mem_loc = self.local_var_mem_loc(&var);
                        self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::Parameter(par) => {
                        let ptr_wasm_loc = self.param_wasm_loc(&par);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::ReturnValue(ret_val) => {
                        let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::TemporaryStackValue(tmp_val) => {
                        let mem_val = tmp_val.mem_stack_val();
                        self.mem_frame.borrow_mut().gen_load_value_addr(&mem_val, byte_offset);
                    }
                };
                return WASMType::PTR;
            },
            CircomStackValueKind::ArrayElementRef(arr, offset) => {
                match arr {
                    CircomValueRef::LocalVariable(var) => {
                        let mem_loc = self.local_var_mem_loc(&var);
                        self.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
                        let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::Parameter(par) => {
                        let ptr_wasm_loc = self.param_wasm_loc(&par);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                        let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::ReturnValue(ret_val) => {
                        let ptr_wasm_loc = self.ret_val_wasm_loc(&ret_val);
                        self.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
                        let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                        self.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
                        self.func.borrow_mut().gen_add(WASMType::PTR);
                    }
                    CircomValueRef::TemporaryStackValue(tmp_val) => {
                        let mem_val = tmp_val.mem_stack_val();
                        let byte_offset = offset * CircomValueType::FR.size(self.size_32_bit);
                        self.mem_frame.borrow_mut().gen_load_value_addr(&mem_val, byte_offset);
                        return WASMType::PTR;
                    }
                };
                return WASMType::PTR;
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
                    CircomStackValueKind::TemporaryStackValue(..) => {
                        // deallocating space on top of memory stack
                        mem_stack_drop_count += 1;
                    }
                    CircomStackValueKind::ValueRef(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::ArraySliceRef(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::ArrayElementRef(..) => {
                        // doing nothing
                    }
                    CircomStackValueKind::WASMLocal(..) => {
                        // doing nothing
                    }
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

    /// Dumps value reference to string
    fn dump_value_ref(&self, val: &CircomValueRef) -> String {
        return match val {
            CircomValueRef::LocalVariable(loc_var) => {
                format!("LOCAL {}", self.local_var_name(loc_var))
            }
            CircomValueRef::Parameter(par) => {
                format!("PARAM {}", self.param_name(par))
            }
            CircomValueRef::ReturnValue(ret_val) => {
                format!("RETVAL {}", self.ret_val_idx(ret_val))
            }
            CircomValueRef::TemporaryStackValue(stack_val) => {
                format!("STACK #{}", stack_val.dump_ref())
            }
        };
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
                    CircomStackValueKind::TemporaryStackValue(temp_val) => {
                        format!("STACK {}", temp_val.dump())
                    }
                    CircomStackValueKind::ValueRef(val_ref) => {
                        let value_str = match val_ref {
                            CircomValueRef::LocalVariable(var) => {
                                let var_loc = self.local_var_mem_loc(var);
                                let loc_str = self.mem_frame.borrow().dump_local(var_loc);
                                format!("LOCAL {} {} {}",
                                        self.local_var_name(var),
                                        self.local_var_type(var).to_string(),
                                        loc_str)
                            }   
                            CircomValueRef::Parameter(par) => {
                                let loc = self.param_wasm_loc(par);
                                let name = self.param_name(par);
                                let type_str = self.param_type(par).to_string();
                                let wasm_loc_str = self.func.borrow().gen_local_ref(loc);
        
                                format!("PARAM {} {} {}", name, type_str, wasm_loc_str)
                            }
                            CircomValueRef::ReturnValue(ret_val) => {
                                let loc = self.ret_val_wasm_loc(ret_val);
                                let name = self.ret_val_idx(ret_val).to_string();
                                let type_str = self.ret_val_type(ret_val).to_string();
                                let wasm_loc_str = self.func.borrow().gen_local_ref(loc);
        
                                format!("RETVAL {} {} {}", name, type_str, wasm_loc_str)
                            }
                            CircomValueRef::TemporaryStackValue(temp_val) => {
                                format!("STACK {}", temp_val.dump())
                            }
                        };
                        
                        format!("REF {}", value_str)
                    }
                    CircomStackValueKind::ArraySliceRef(arr, offset, size) => {
                        format!("REF ({})[{}, {}) FR[{}]",
                                self.dump_value_ref(arr),
                                offset,
                                offset + size,
                                size)
                    }
                    CircomStackValueKind::ArrayElementRef(arr, offset) => {
                        format!("REF ({})[{}], FR", self.dump_value_ref(arr), offset)
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
