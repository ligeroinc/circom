
use crate::ligetron_elements::wasm;

use super::memory_stack::*;
use super::module::*;
use super::types::*;
use super::wasm::*;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;


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

impl CircomValueRef {
    /// Generates loading of value ptr onto WASM stack
    pub fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        match self {
            CircomValueRef::LocalVariable(var) => {
                var.load_ptr_to_wasm_stack(frame);
            }
            CircomValueRef::Parameter(par) => {
                par.load_ptr_to_wasm_stack(frame);
            }
            CircomValueRef::ReturnValue(ret_val) => {
                ret_val.load_ptr_to_wasm_stack(frame);
            }
            CircomValueRef::TemporaryStackValue(val) => {
                val.load_ptr_to_wasm_stack(frame);
            }
        }
    }

    /// Generates loading of array element ptr onto WASM stack
    pub fn load_array_element_ptr_to_wasm_stack(&self,
                                                frame: &mut CircomStackFrame,
                                                index: usize) {
        match self {
            CircomValueRef::LocalVariable(var) => {
                var.gen_addr_const(frame, index);
            }
            CircomValueRef::Parameter(par) => {
                par.gen_addr_const(frame, index);
            }
            CircomValueRef::ReturnValue(ret_val) => {
                ret_val.gen_addr_const(frame, index);
            }
            CircomValueRef::TemporaryStackValue(val) => {
                val.gen_addr_const(frame, index);
            }
        }
    }
}


pub trait ConvertibleToValueRef {
    /// Converts value to reference
    fn to_ref(&self) -> CircomValueRef;

    /// Generates loading of value onto WASM stack
    fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame);

    /// Generates address of array element at specified constant index
    fn gen_addr_const(&self, frame: &mut CircomStackFrame, index: usize) {
        self.load_ptr_to_wasm_stack(frame);

        let byte_offset = index * CircomValueType::FR.size();
        if byte_offset != 0 {
            frame.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
            frame.func.borrow_mut().gen_add(WASMType::PTR);
        }
    }

    /// Generates address of array element at dynamic index
    fn gen_addr_dyn(&self, frame: &mut CircomStackFrame, offset: usize) {
        // calculating byte offset from index located in WASM stack
        if offset != 0 {
            frame.func.borrow_mut().gen_const(WASMType::I32, offset as i64);
            frame.func.borrow_mut().gen_add(WASMType::I32);
        }
        let fr_byte_size = CircomValueType::FR.size();
        frame.func.borrow_mut().gen_const(WASMType::I32, fr_byte_size as i64);
        frame.func.borrow_mut().gen_mul(WASMType::I32);

        // calculating value address
        self.load_ptr_to_wasm_stack(frame);
        frame.func.borrow_mut().gen_add(WASMType::PTR);
    }
}

impl ConvertibleToValueRef for CircomLocalVariableRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::LocalVariable(self.clone());
    }

    /// Generates address of value
    fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let mem_loc = frame.local_var_mem_loc(&self);
        frame.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);
    }
}

impl ConvertibleToValueRef for CircomParameterRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::Parameter(self.clone());
    }

    /// Generates address of value
    fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let ptr_wasm_loc = frame.param_wasm_loc(self);
        frame.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
    }
}

impl ConvertibleToValueRef for CircomReturnValueRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::ReturnValue(self.clone());
    }

    /// Generates address of value
    fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let ptr_wasm_loc = frame.ret_val_wasm_loc(self);
        frame.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
    }
}

impl ConvertibleToValueRef for TemporaryStackValueRef {
    fn to_ref(&self) -> CircomValueRef {
        return CircomValueRef::TemporaryStackValue(self.clone());
    }

    /// Generates address of value
    fn load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        frame.mem_frame.borrow_mut().gen_load_value_addr(self.mem_stack_val(), 0);
    }

    /// Generates address of array element at specified constant index
    fn gen_addr_const(&self, frame: &mut CircomStackFrame, index: usize) {
        let byte_offset = index * CircomValueType::FR.size();
        frame.mem_frame.borrow_mut().gen_load_value_addr(self.mem_stack_val(), byte_offset);
    }

    /// Generates address of array element at dynamic index
    fn gen_addr_dyn(&self, _frame: &mut CircomStackFrame, _offset: usize) {
        panic!("NYI");
    }
}


/// Location of value stored on Circom stack frame
#[derive(Clone)]
pub enum CircomStackValueKind {
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

    /// Value stored in WASM stack
    WASMSTack(WASMType),

    /// Constant index in array
    ConstIndex(i32),

    /// Dynamic index in array with constant offset (located in WASM stack as I32 value)
    Index(i32),

    /// Dynamic address value located in WASM stack as PTR value
    Address,

    /// Dynamic array address value located in WASM stack as PTR value
    ArrayAddress(usize),
}


/// Value located in Circom stack frame
struct CircomStackValue {
    /// Value kind
    kind: CircomStackValueKind,
}

impl CircomStackValue {
    /// Creates new stack value
    fn new(kind: CircomStackValueKind) -> CircomStackValue {
        return CircomStackValue {
            kind: kind
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
    fn new(value_rc: Rc<RefCell<CircomStackValue>>) -> CircomStackValueRef {
        return CircomStackValueRef {
            value_rc: value_rc
        };
    }
}


/// Represents Circom stack frame. Contains logic for managing current logical
/// state of stack and generating code for stack values manipulation.
pub struct CircomStackFrame {
    /// Reference to parent Circom module
    module: Rc<RefCell<CircomModule>>,

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
    branch_starts: Vec<usize>,

    /// WASM locals for temporary values
    temp_wasm_locals: HashMap<WASMType, WASMLocalVariableRef>
}

impl CircomStackFrame {
    /// Creates new stack frame
    pub fn new(module: Rc<RefCell<CircomModule>>,
               func: Rc<RefCell<WASMFunction>>,
               mem_frame: Rc<RefCell<MemoryStackFrame>>) -> CircomStackFrame {
        return CircomStackFrame {
            module: module,
            func: func,
            mem_frame: mem_frame,
            locals: vec![],
            params: vec![],
            ret_vals: vec![],
            values: vec![],
            branch_starts: vec![],
            temp_wasm_locals: HashMap::new()
        };
    }


    ////////////////////////////////////////////////////////////
    /// Stack values properties and functions

    /// Returns stack value kind
    pub fn value_kind(&self, val_ref: &CircomStackValueRef) -> CircomStackValueKind {
        return val_ref.value_rc.borrow().kind.clone();
    }

    /// Returns true if value is located in WASM stack
    pub fn value_is_wasm_stack(&self, val_ref: &CircomStackValueRef) -> bool {
        return match val_ref.value_rc.borrow().kind {
            CircomStackValueKind::MemoryPtrConst(..) => false,
            CircomStackValueKind::TemporaryStackValue(..) => false,
            CircomStackValueKind::ValueRef(..) => false,
            CircomStackValueKind::ArraySliceRef(..) => false,
            CircomStackValueKind::ArrayElementRef(..) => false,
            CircomStackValueKind::WASMSTack(..) => true,
            CircomStackValueKind::ConstIndex(..) => false,
            CircomStackValueKind::Index(..) => true,
            CircomStackValueKind::Address => true,
            CircomStackValueKind::ArrayAddress(..) => true
        };
    }

    /// Returns true if value is located in memory stack
    pub fn value_is_mem_stack(&self, val_ref: &CircomStackValueRef) -> bool {
        match val_ref.value_rc.borrow().kind {
            CircomStackValueKind::TemporaryStackValue(..) => true,
            _ => false
        }
    }

    /// Returns stack value type
    pub fn value_type(&self, val_ref: &CircomStackValueRef) -> CircomValueType {
        let val_kind = &val_ref.value_rc.borrow().kind;
        return match val_kind {
            CircomStackValueKind::MemoryPtrConst(tp, _) => tp.clone(),
            CircomStackValueKind::TemporaryStackValue(temp_val) => temp_val.tp().clone(),
            CircomStackValueKind::ValueRef(val_ref) => self.value_ref_type(val_ref).clone(),
            CircomStackValueKind::ArraySliceRef(_arr, _offset, size) => {
                CircomValueType::FRArray(*size)
            },
            CircomStackValueKind::ArrayElementRef(_arr, _offset) => {
                CircomValueType::FR
            },
            CircomStackValueKind::WASMSTack(wasm_tp) => CircomValueType::WASM(*wasm_tp),
            CircomStackValueKind::ConstIndex(..) => {
                panic!("should not reach here")
            },
            CircomStackValueKind::Index(..) => {
                panic!("should not reach here");
            },
            CircomStackValueKind::Address => {
                CircomValueType::FR
            }
            CircomStackValueKind::ArrayAddress(size) => {
                CircomValueType::FRArray(*size)
            }
        };
    }

    /// Performs additional actions for deallocating value
    fn value_deallocate(&mut self, val: &CircomStackValueRef) {
        match &val.value_rc.borrow().kind {
            CircomStackValueKind::TemporaryStackValue(val) => {
                match val.tp() {
                    CircomValueType::FR => {
                        self.load_ref(val);
                        let fp256_clear =
                            self.module.borrow().ligetron().fp256_clear.clone();
                        self.gen_call(&fp256_clear);
                    }
                    CircomValueType::FRArray(size) => {
                        for i in 0 .. *size {
                            self.load_const_index(i as i32);
                            self.load_array_element_ref(val);
                            let fp256_clear =
                                self.module.borrow().ligetron().fp256_clear.clone();
                            self.gen_call(&fp256_clear);
                        }
                    }
                    _ => {}
                };
            }
            _ => {}
        }
    }

    /// Loads value pointer onto WASM stack
    pub fn value_load_ptr_to_wasm_stack(&mut self, val: &CircomStackValueRef) {
        if self.value_is_wasm_stack(&val) {
            panic!("value is already located on WASM stack");
        }

        match self.value_kind(&val) {
            CircomStackValueKind::MemoryPtrConst(_tp, addr) => {
                self.func.borrow_mut().gen_const(WASMType::PTR, addr as i64);
            }
            CircomStackValueKind::TemporaryStackValue(temp_val) => {
                temp_val.load_ptr_to_wasm_stack(self);
            }
            CircomStackValueKind::ValueRef(val) => {
                val.load_ptr_to_wasm_stack(self);
            }
            CircomStackValueKind::ArraySliceRef(arr, offset, _size) => {
                arr.load_array_element_ptr_to_wasm_stack(self, offset);
            }
            CircomStackValueKind::ArrayElementRef(arr, offset) => {
                arr.load_array_element_ptr_to_wasm_stack(self, offset);
            }
            _ => {
                // should be handled by WASM check at the beginning of function
                panic!("should not reach here");
            }
        }
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

    /// Dumps stack value to string
    fn value_dump(&self, val: &CircomStackValueRef) -> String {
        match &val.value_rc.borrow().kind {
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
            CircomStackValueKind::WASMSTack(tp) => {
                format!("WASM STACK {}", tp.to_string())
            }
            CircomStackValueKind::ConstIndex(addr) => {
                format!("INDEX CONST {}", addr)
            }
            CircomStackValueKind::Index(offset) => {
                format!("INDEX WASM STACK + {}", offset)
            }
            CircomStackValueKind::Address => {
                format!("ADDRESS FR")
            }
            CircomStackValueKind::ArrayAddress(size) => {
                format!("ADDRESS FR[{}]", size)
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Stack manipulation

    /// Returns stack size
    fn size(&self) -> usize {
        return self.values.len();
    }

    /// Pushes value to stack
    fn push(&mut self, kind: CircomStackValueKind) -> CircomStackValueRef {
        let value = CircomStackValue::new(kind);
        let value_rc = Rc::new(RefCell::new(value));
        self.values.push(value_rc.clone());
        return CircomStackValueRef::new(value_rc);
    }

    /// Pops values from stack
    pub fn pop(&mut self, count: usize) {
        let mut mem_stack_drop_count: usize = 0;

        for _ in 0 .. count {
            {
                let val = self.top(0);

                if self.value_is_mem_stack(&val) {
                    // incrementing number of memory stack values to deallocate
                    mem_stack_drop_count += 1;
                }

                // performing additional deallocating actions for value
                self.value_deallocate(&val);
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

    /// Drops specified number of values located on top of stack
    pub fn drop(&mut self, count: usize) {
        // removing values from vector of values and calculating number of required operations
        let mut wasm_stack_drop_count: usize = 0;
        let mut mem_stack_drop_count: usize = 0;
        for _ in 0 .. count {
            {
                let val = self.top(0);

                if self.value_is_mem_stack(&val) {
                    // dropping value from top of memory stack
                    mem_stack_drop_count += 1;
                }

                if self.value_is_wasm_stack(&val) {
                    // dropping value from wasm stack
                    wasm_stack_drop_count += 1;
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


    ////////////////////////////////////////////////////////////
    /// Local variables

    /// Returns sequence of local variables
    pub fn locals(&self) -> impl Iterator<Item = CircomLocalVariableRef> {
        return (0 .. self.locals.len()).map(|idx| {
            CircomLocalVariableRef::new(idx)
        });
    }

    /// Creates new local variable in stack frame
    pub fn new_local_var(&mut self, name: String, tp: CircomValueType) -> CircomLocalVariableRef {
        // allocating memory stack local
        let mem_loc = self.mem_frame.borrow_mut().new_local(tp.size());

        // creating local vairable
        let var_idx = self.locals.len();
        let loc = CircomLocalVariable::new(name, tp, mem_loc);
        self.locals.push(loc);

        return CircomLocalVariableRef::new(var_idx);
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

    /// Allocates multiple temporary values on stack. Returns references to allocated values
    pub fn alloc_temp_n(&mut self, types: &Vec<CircomValueType>) -> Vec<TemporaryStackValueRef> {
        // allocating space on memory stack
        let sizes = types.iter().map(|t| t.size()).collect();
        let mem_stack_vals = self.mem_frame.borrow_mut().alloc(sizes);

        let mut vals = Vec::<TemporaryStackValueRef>::new();
        for (idx, mem_val) in mem_stack_vals.iter().enumerate() {
            let temp_val = TemporaryStackValueRef::new(types[idx].clone(), mem_val.clone());

            // adding value onto logical stack
            self.push(CircomStackValueKind::TemporaryStackValue(temp_val.clone()));

            // initializing FR value in Ligetron
            match &types[idx] {
                CircomValueType::FR => {
                    self.load_ref(&temp_val);
                    let fp256_init = self.module.borrow().ligetron().fp256_init.clone();
                    self.gen_call(&fp256_init);
                }
                CircomValueType::FRArray(size) => {
                    for i in 0 .. *size {
                        self.load_const_index(i as i32);
                        self.load_array_element_ref(&temp_val);
                        let fp256_init = self.module.borrow().ligetron().fp256_init.clone();
                        self.gen_call(&fp256_init);
                    }
                }
                _ => {}
            }

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

    /// Loads pointer to temporary value onto WASM stack
    pub fn load_temp_value_ptr_to_wasm_stack(&mut self, val: &TemporaryStackValueRef) {
        val.load_ptr_to_wasm_stack(self);
    }

    /// Loads pointer to temporary array value element onto WASM stack
    pub fn load_temp_value_array_element_ptr_to_wasm_stack(&mut self,
                                                           val: &TemporaryStackValueRef,
                                                           index: usize) {
        val.gen_addr_const(self, index);
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
    pub fn load_array_slice_ref<T: ConvertibleToValueRef>(&mut self, arr: &T, size: usize) {
        let arr_ref = arr.to_ref();

        // checking parameter type
        match self.value_ref_type(&arr_ref) {
            CircomValueType::FRArray(..) => {
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(value) => {
                self.pop(1);
                self.push(CircomStackValueKind::ArraySliceRef(arr.to_ref(), value as usize, size));
            }
            CircomStackValueKind::Index(offset) => {
                arr.gen_addr_dyn(self, offset as usize);

                self.pop(1);
                self.push(CircomStackValueKind::Address);
            }
            _ => {
                panic!("top stack value is not an index");
            }
        }
    }

    /// Loads reference to element of array to stack
    pub fn load_array_element_ref<T: ConvertibleToValueRef>(&mut self, arr: &T) {
        let arr_ref = arr.to_ref();

        // checking array variable type
        match self.value_ref_type(&arr_ref) {
            CircomValueType::FRArray(..) => {},
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(value) => {
                self.pop(1);
                self.push(CircomStackValueKind::ArrayElementRef(arr.to_ref(), value as usize));
            }
            CircomStackValueKind::Index(offset) => {
                arr.gen_addr_dyn(self, offset as usize);

                self.pop(1);
                self.push(CircomStackValueKind::Address);
            }
            _ => {
                panic!("top stack value is not an index");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Indexes and Addresses

    /// Loads const address on stack
    pub fn load_const_index(&mut self, addr: i32) {
        self.push(CircomStackValueKind::ConstIndex(addr));
    }

    /// Converts current stack top value to index. The top of stack must be a WASM I32 value
    pub fn convert_index(&mut self) {
        // checking value type
        match &self.values.last().expect("stack is empty").borrow().kind {
            CircomStackValueKind::WASMSTack(tp) => {
                if *tp != WASMType::I32 {
                    panic!("top stack value is not a WASM I32 stack value");
                }
            }
            _ => {
                panic!("top stack value is not a WASM stack value");
            }
        }

        // converting stack value
        self.pop(1);
        self.push(CircomStackValueKind::Index(0));
    }

    /// Generates index add operation
    pub fn index_add(&mut self) {
        let first = self.top(0);
        let second = self.top(1);

        let res = match &first.value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(addr) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstIndex(addr2) => {
                        CircomStackValueKind::ConstIndex(addr + addr2)
                    }
                    CircomStackValueKind::Index(offset) => {
                        CircomStackValueKind::Index(addr + offset)
                    }
                    _ => {
                        panic!("stack value is not an address");
                    }
                }
            }
            CircomStackValueKind::Index(offset) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstIndex(addr) => {
                        CircomStackValueKind::Index(offset + addr)
                    }
                    CircomStackValueKind::Index(offset2) => {
                        self.func.borrow_mut().gen_add(WASMType::I32);
                        CircomStackValueKind::Index(offset + offset2)
                    }
                    _ => {
                        panic!("stack value is not an address");
                    }
                }
            }
            _ => {
                panic!("stack value is not an address or WASM constant");
            }
        };

        self.pop(2);
        self.push(res);
    }

    /// Generates index mul operation
    pub fn index_mul(&mut self) {
        let first = self.top(0);
        let second = self.top(1);

        let res = match &first.value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(addr) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstIndex(addr2) => {
                        CircomStackValueKind::ConstIndex(addr * addr2)
                    }
                    CircomStackValueKind::Index(offset) => {
                        self.func.borrow_mut().gen_const(WASMType::I32, *addr as i64);
                        self.func.borrow_mut().gen_mul(WASMType::I32);
                        CircomStackValueKind::Index(offset * *addr)
                    }
                    _ => {
                        panic!("stack value is not an address");
                    }
                }
            }
            CircomStackValueKind::Index(offset) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstIndex(addr) => {
                        self.func.borrow_mut().gen_const(WASMType::I32, *addr as i64);
                        self.func.borrow_mut().gen_mul(WASMType::I32);
                        CircomStackValueKind::Index(offset * *addr)
                    }
                    CircomStackValueKind::Index(offset) => {
                        // do we need this case?
                        panic!("NYI");
                    }
                    _ => {
                        panic!("stack value is not an address");
                    }
                }
            }
            _ => {
                panic!("stack value is not an address or WASM constant");
            }
        };

        self.pop(2);
        self.push(res);
    }

    /// Returns true if index located on top of stack is const
    pub fn top_index_is_const(&self) -> bool {
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(..) => true,
            CircomStackValueKind::Index(..) => false,
            _ => {
                panic!("top stack value is not and index value");
            }
        }
    }

    /// Returns constant offset of index located on top of stack
    pub fn top_index_offset(&self) -> i32 {
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstIndex(value) => value,
            CircomStackValueKind::Index(offset) => offset,
            _ => {
                panic!("top stack value is not and index value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Call operation
    
    /// Loads specified number of top stack values onto WASM stack
    pub fn load_values_to_wasm_stack(&mut self, count: usize) {
        if count == 0 {
            return;
        }

        // We need to put all parameters onto WASM stack in correct order. For that,
        // we first store all parameters located on WASM stack to temporary locals and
        // then load them again

        let mut params_locals = Vec::<Option<WASMLocalVariableRef>>::new();
        for _ in 0 .. count {
            params_locals.push(None);
        }

        // saving parameters already located on WASM stack to WASM locals 
        for val_idx in 0 .. count {
            let val = self.top(val_idx);
            if self.value_is_wasm_stack(&val) {
                let val_type = self.value_type(&val);
                let wasm_t = match val_type {
                    CircomValueType::WASM(wt) => wt,
                    CircomValueType::FR => WASMType::PTR,
                    CircomValueType::FRArray(..) => WASMType::PTR
                };

                // saving parameter from WASM stack to temporary local
                let tmp_local = self.func.borrow_mut().new_wasm_local(wasm_t);
                self.func.borrow_mut().gen_local_set(&tmp_local);
                params_locals[val_idx] = Some(tmp_local);
            }
        }

        // loading parameters onto WASM stack
        for param_idx in 0 .. count {
            let val_idx = count - param_idx - 1;
            match &params_locals[val_idx] {
                Some(wasm_local) => {
                    self.func.borrow_mut().gen_local_get(&wasm_local);
                }
                None => {
                    // loading parameter onto WASM stack using generic load
                    self.value_load_ptr_to_wasm_stack(&self.top(val_idx));
                }
            }
        }
    }

    /// Generates call to function passing values located on top of stack as parameters
    pub fn gen_call(&mut self, func_ref: &CircomFunctionRef) {
        // first calculating number of stack values used in this call
        let mut num_stack_vals: usize = func_ref.tp().params().len();
        for ret_type in func_ref.tp().ret_types().iter().rev() {
            match ret_type {
                CircomValueType::WASM(_) => {
                    // WASM return values don't require stack parameter
                },
                CircomValueType::FR => {
                    num_stack_vals += 1;
                },
                CircomValueType::FRArray(_) => {
                    num_stack_vals += 1;
                }
            }
        }

        self.load_values_to_wasm_stack(num_stack_vals);
 
        // Generating call instruction
        self.func.borrow_mut().gen_call(&func_ref.to_wasm());

        // removing parameter values from stack
        self.pop(num_stack_vals);

        // adding WASM return values to stack
        for ret_type in func_ref.tp().ret_types() {
            match ret_type {
                CircomValueType::WASM(wasm_type) => {
                    self.push_wasm_stack(wasm_type.clone());
                },
                _ => {}
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Constants

    /// Loads WASM const value on stack
    pub fn load_const(&mut self, tp: WASMType, val: i64) -> CircomStackValueRef {
        self.func.borrow_mut().gen_const(tp, val);
        return self.push(CircomStackValueKind::WASMSTack(tp));
    }

    /// Loads value stored in global memory at constant address
    pub fn load_mem_const(&mut self,
                          tp: CircomValueType,
                          addr: usize) -> CircomStackValueRef {
        self.func.borrow_mut().gen_const(WASMType::PTR, addr as i64);
        return self.push(CircomStackValueKind::MemoryPtrConst(tp, addr));
    }


    ////////////////////////////////////////////////////////////
    /// WASM stack manipulation

    /// Pushes value stored in WASM stack
    pub fn push_wasm_stack(&mut self, tp: WASMType) -> CircomStackValueRef {
        return self.push(CircomStackValueKind::WASMSTack(tp));
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


    ////////////////////////////////////////////////////////////
    /// Branches

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


    ////////////////////////////////////////////////////////////
    /// Function entry and exit

    /// Generates function entry code
    pub fn gen_func_entry(&mut self) {
        let stack_rc = Rc::new(RefCell::new(WASMStackState::new()));
        let mut igen = InstructionGenerator::new(self.module.borrow().wasm_module_rc().clone(),
                                                 self.func.borrow().frame_rc().clone(),
                                                 stack_rc);
        igen.gen_comment("initializing Fr local variables");
        for loc in &self.locals {
            match loc.type_ {
                CircomValueType::FR => {
                    self.mem_frame.borrow_mut().gen_load_local_addr_igen(&loc.loc, 0, &mut igen);
                    igen.gen_call(&self.module.borrow().ligetron().fp256_init.to_wasm());
                }
                CircomValueType::FRArray(size) => {
                    for idx in 0 .. size {
                        let offset = CircomValueType::FR.size() * idx;
                        self.mem_frame.borrow_mut().gen_load_local_addr_igen(&loc.loc,
                                                                             offset,
                                                                             &mut igen);
                        igen.gen_call(&self.module.borrow().ligetron().fp256_init.to_wasm());
                    }
                }
                _ => {}
            }
        }

        self.func.borrow_mut().inst_gen_mut().insert_insts_begin(&igen);


        self.mem_frame.borrow_mut().gen_func_entry();
    }

    /// Generates function exit code
    pub fn gen_func_exit(&mut self) {
        self.func.borrow_mut().gen_comment("deinitializing Fr local variables");
        for loc in self.locals() {
            match self.local_var_type(&loc) {
                CircomValueType::FR => {
                    self.load_ref(&loc);
                    let fp256_clear = &self.module.borrow().ligetron().fp256_clear.clone();
                    self.gen_call(&fp256_clear);
                }
                CircomValueType::FRArray(size) => {
                    for idx in 0 .. *size {
                        self.load_const_index(idx as i32);
                        self.load_array_element_ref(&loc);
                        let fp256_clear = &self.module.borrow().ligetron().fp256_clear.clone();
                        self.gen_call(&fp256_clear);
                    }
                }
                _ => {}
            }
        }

        self.mem_frame.borrow_mut().gen_func_exit();
    }


    ////////////////////////////////////////////////////////////
    /// Utilities

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
        let mut res = String::new();
        for idx in 0 .. self.size() {
            let val = self.top(self.size() - idx - 1);
            let val_str = self.value_dump(&val);

            if !res.is_empty() {
                res += "\n";
            }

            res += &format!("{:3}\t{}", idx, val_str);
        }

        return res;
    }
}
