
use crate::ligetron_elements::wasm;

use super::arrays::*;
use super::memory_stack::*;
use super::module::*;
use super::types::*;
use super::wasm::*;

use std::cell::RefCell;
use std::cell::RefMut;
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


pub trait CircomValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef>;

    /// Dumps reference information to string in short form
    fn dump_ref(&self, frame: &CircomStackFrame) -> String;

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String;

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType;

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame);

    /// Generates loading of value ptr with specified constant offset onto WASM stack
    fn gen_load_ptr_with_offset_to_wasm_stack(&self, frame:
                                              &mut CircomStackFrame,
                                              byte_offset: usize) {
        self.gen_load_ptr_to_wasm_stack(frame);

        if byte_offset != 0 {
            frame.func.borrow_mut().gen_const(WASMType::I32, byte_offset as i64);
            frame.func.borrow_mut().gen_add(WASMType::PTR);
        }
    }

    /// Generates loading of address of array element at dynamic index onto WASM stack
    fn gen_load_array_element_ptr_to_wasm_stack_dyn(&self, frame:
                                                    &mut CircomStackFrame,
                                                    offset: usize) {
        // calculating byte offset from index located in WASM stack
        if offset != 0 {
            frame.func.borrow_mut().gen_const(WASMType::I32, offset as i64);
            frame.func.borrow_mut().gen_add(WASMType::I32);
        }
        let fr_byte_size = CircomValueType::FR.size();
        frame.func.borrow_mut().gen_const(WASMType::I32, fr_byte_size as i64);
        frame.func.borrow_mut().gen_mul(WASMType::I32);

        // calculating value address
        self.gen_load_ptr_to_wasm_stack(frame);
        frame.func.borrow_mut().gen_add(WASMType::PTR);
    }
}


impl CircomValueRef for CircomLocalVariableRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("LOCAL {}", frame.local_var_name(self));
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        let var_loc = frame.local_var_mem_loc(self);
        let loc_str = frame.mem_frame.borrow().dump_local(var_loc);
        format!("LOCAL {} {} {}",
                frame.local_var_name(self),
                frame.local_var_type(self).to_string(),
                loc_str)
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        return frame.local_var_type(self).clone();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let mem_loc = frame.local_var_mem_loc(&self);
        frame.mem_frame.borrow_mut().gen_load_local_addr(&mem_loc);

    }
}


impl CircomValueRef for CircomParameterRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("PARAM {}", frame.param_name(self));
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        let loc = frame.param_wasm_loc(self);
        let name = frame.param_name(self);
        let type_str = frame.param_type(self).to_string();
        let wasm_loc_str = frame.func.borrow().gen_local_ref(loc);

        format!("PARAM {} {} {}", name, type_str, wasm_loc_str)
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        return frame.param_type(self).clone();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let ptr_wasm_loc = frame.param_wasm_loc(self);
        frame.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
    }
}


impl CircomValueRef for CircomReturnValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("RETVAL {}", frame.ret_val_idx(self));
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        let loc = frame.ret_val_wasm_loc(self);
        let name = frame.ret_val_idx(self).to_string();
        let type_str = frame.ret_val_type(self).to_string();
        let wasm_loc_str = frame.func.borrow().gen_local_ref(loc);

        format!("RETVAL {} {} {}", name, type_str, wasm_loc_str)
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        return frame.ret_val_type(self).clone();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        let ptr_wasm_loc = frame.ret_val_wasm_loc(self);
        frame.func.borrow_mut().gen_local_get(&ptr_wasm_loc);
    }
}


impl CircomValueRef for TemporaryStackValueRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK #{}", self.dump_ref());
    }

    /// Dumps value to string
    fn dump(&self, _frame: &CircomStackFrame) -> String {
        return format!("STACK {}", self.dump());
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return self.tp().clone();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self, frame: &mut CircomStackFrame) {
        frame.mem_frame.borrow_mut().gen_load_value_addr(self.mem_stack_val(), 0);
    }

    /// Generates loading of value ptr with specified constant offset onto WASM stack
    fn gen_load_ptr_with_offset_to_wasm_stack(&self, frame:
                                              &mut CircomStackFrame,
                                              byte_offset: usize) {
        frame.mem_frame.borrow_mut().gen_load_value_addr(self.mem_stack_val(), byte_offset);
    }
}


impl Clone for Box<dyn CircomValueRef> {
    fn clone(&self) -> Box<dyn CircomValueRef> {
        return self.clone_ref();
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
    ValueRef(Box<dyn CircomValueRef>),

    /// Value stored in WASM stack
    WASMSTack(WASMType),

    /// Constant i32 value
    ConstValue(i32),

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
    pub func: Rc<RefCell<WASMFunction>>,

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
            CircomStackValueKind::WASMSTack(..) => true,
            CircomStackValueKind::ConstValue(..) => false,
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
            CircomStackValueKind::ValueRef(val_ref) => val_ref.xtype(self).clone(),
            CircomStackValueKind::WASMSTack(wasm_tp) => CircomValueType::WASM(*wasm_tp),
            CircomStackValueKind::ConstValue(..) => {
                CircomValueType::WASM(WASMType::I32)
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

    /// Loads value onto WASM stack converting it if required
    pub fn value_load_as_wasm(&self, val_ref: &CircomStackValueRef) {
        let val_kind = &val_ref.value_rc.borrow().kind;
        return match val_kind {
            CircomStackValueKind::MemoryPtrConst(_tp, _addr) => {
                panic!("Conversion from global constants to i32 is not implemented yet");
            }
            CircomStackValueKind::TemporaryStackValue(_temp_val) => {
                panic!("")
            }
            CircomStackValueKind::ValueRef(_val_ref) => {
            }
            CircomStackValueKind::WASMSTack(_wasm_tp) => {
            }
            CircomStackValueKind::ConstValue(..) => {
            }
            CircomStackValueKind::Index(..) => {
                panic!("should not reach here");
            }
            CircomStackValueKind::Address => {
            }
            CircomStackValueKind::ArrayAddress(_size) => {
            }
        };
    }

    /// Performs additional actions for deallocating value
    fn value_deallocate(&mut self, val: &CircomStackValueRef) {
        match &val.value_rc.borrow().kind {
            CircomStackValueKind::TemporaryStackValue(val) => {
                match val.tp() {
                    CircomValueType::FR => {
                        self.load_ref(val.clone());
                        let fp256_clear =
                            self.module.borrow().ligetron().fp256_clear.clone();
                        self.gen_call(&fp256_clear);
                    }
                    CircomValueType::FRArray(size) => {
                        for i in 0 .. *size {
                            self.load_i32_const(i as i32);
                            self.load_array_element_ref(val.clone());
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

    /// Loads int value to WASM stack
    pub fn value_load_int_to_wasm_stack(&mut self, val: &CircomStackValueRef) {
        match self.value_kind(&val) {
            CircomStackValueKind::ConstValue(val) => {
                self.func.borrow_mut().gen_const(WASMType::I32, val as i64);
            }
            CircomStackValueKind::WASMSTack(_type) => {
                panic!("value is already located on WASM stack");
            }
            CircomStackValueKind::ValueRef(val_ref) => {
                let var_type = val_ref.xtype(self).clone();
                match var_type {
                    CircomValueType::WASM(wasm_type) => {
                        val_ref.gen_load_ptr_to_wasm_stack(self);
                        self.func.borrow_mut().gen_load(wasm_type.clone());
                    }
                    _ => {
                        panic!("Fr local variable can't be loaded as int to WASM stack");
                    }
                }
            }
            _ => {
                panic!("value can't be loaded as int to WASM stack");
            }
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
                temp_val.gen_load_ptr_to_wasm_stack(self);
            }
            CircomStackValueKind::ValueRef(val) => {
                val.gen_load_ptr_to_wasm_stack(self);
            }
            _ => {
                panic!("should not reach here");
            }
        }
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
                format!("REF {}", val_ref.dump(self))
            }
            CircomStackValueKind::WASMSTack(tp) => {
                format!("WASM STACK {}", tp.to_string())
            }
            CircomStackValueKind::ConstValue(val) => {
                format!("CONST {}", val)
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
        if types.len() == 0 {
            return Vec::new();
        }

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
                    self.load_ref(temp_val.clone());
                    let fp256_init = self.module.borrow().ligetron().fp256_init.clone();
                    self.gen_call(&fp256_init);
                }
                CircomValueType::FRArray(size) => {
                    for i in 0 .. *size {
                        self.load_i32_const(i as i32);
                        self.load_array_element_ref(temp_val.clone());
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
        val.gen_load_ptr_to_wasm_stack(self);
    }


    ////////////////////////////////////////////////////////////
    /// References

    /// Loads reference to value on stack
    pub fn load_ref<T: CircomValueRef + 'static>(&mut self, val: T) {
        self.push(CircomStackValueKind::ValueRef(Box::new(val)));
    }

    /// Loads reference to subarray of array to stack
    pub fn load_array_slice_ref<T: CircomValueRef + 'static>(&mut self, arr: T, size: usize) {
        // checking parameter type
        match arr.xtype(self) {
            CircomValueType::FRArray(..) => {
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstValue(value) => {
                self.pop(1);
                let slice_ref = ArraySliceRef::new(Box::new(arr), value as usize, size);
                self.push(CircomStackValueKind::ValueRef(Box::new(slice_ref)));
            }
            CircomStackValueKind::Index(offset) => {
                gen_load_array_element_ptr_to_wasm_stack_dyn(self, &arr, offset as usize);

                self.pop(1);
                self.push(CircomStackValueKind::Address);
            }
            _ => {
                panic!("top stack value is not an index");
            }
        }
    }

    /// Loads reference to element of array to stack
    pub fn load_array_element_ref<T: CircomValueRef + 'static>(&mut self, arr: T) {
        // checking array variable type
        match arr.xtype(self) {
            CircomValueType::FRArray(..) => {},
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstValue(value) => {
                self.pop(1);
                let elt_ref = ArrayElementRef::new(Box::new(arr), value as usize);
                self.push(CircomStackValueKind::ValueRef(Box::new(elt_ref)));
            }
            CircomStackValueKind::Index(offset) => {
                arr.gen_load_array_element_ptr_to_wasm_stack_dyn(self, offset as usize);
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

    /// Loads i32 const value on stack
    pub fn load_i32_const(&mut self, val: i32) {
        self.push(CircomStackValueKind::ConstValue(val));
    }

    /// Converts current stack top value to index. The top of stack must be a WASM I32 value
    pub fn convert_index(&mut self) {
        // loading value as integer to WASM stack
        self.value_load_int_to_wasm_stack(&self.top(0));

        // converting stack value
        self.pop(1);
        self.push(CircomStackValueKind::Index(0));
    }

    /// Generates index add operation
    pub fn index_add(&mut self) {
        let first = self.top(0);
        let second = self.top(1);

        let res = match &first.value_rc.borrow().kind {
            CircomStackValueKind::ConstValue(addr) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstValue(addr2) => {
                        CircomStackValueKind::ConstValue(addr + addr2)
                    }
                    CircomStackValueKind::Index(offset) => {
                        CircomStackValueKind::Index((*addr as i32) + offset)
                    }
                    _ => {
                        panic!("stack value is not an address");
                    }
                }
            }
            CircomStackValueKind::Index(offset) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstValue(addr) => {
                        CircomStackValueKind::Index(offset + (*addr as i32))
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
            CircomStackValueKind::ConstValue(addr) => {
                match &second.value_rc.borrow().kind {
                    CircomStackValueKind::ConstValue(addr2) => {
                        CircomStackValueKind::ConstValue(addr * addr2)
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
                    CircomStackValueKind::ConstValue(addr) => {
                        self.func.borrow_mut().gen_const(WASMType::I32, *addr as i64);
                        self.func.borrow_mut().gen_mul(WASMType::I32);
                        CircomStackValueKind::Index(offset * *addr)
                    }
                    CircomStackValueKind::Index(_offset) => {
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
            CircomStackValueKind::ConstValue(..) => true,
            CircomStackValueKind::Index(..) => false,
            _ => {
                panic!("top stack value is not and index value");
            }
        }
    }

    /// Returns constant offset of index located on top of stack
    pub fn top_index_offset(&self) -> i32 {
        match self.top(0).value_rc.borrow().kind {
            CircomStackValueKind::ConstValue(value) => value,
            CircomStackValueKind::Index(offset) => offset,
            _ => {
                panic!("top stack value is not and index value");
            }
        }
    }


    ////////////////////////////////////////////////////////////
    /// Call operation

    /// Generates operation with automatic conversion of arguments located on stack to
    /// required types
    pub fn gen_op<Op: FnOnce(RefMut<WASMFunction>) -> ()>
            (&mut self, arg_types: Vec<CircomValueType>, first_is_ptr: bool, op: Op) {
        // We need to put all values onto WASM stack in correct order. For that,
        // we first store all values already located on WASM stack to temporary locals and
        // then load them again

        let mut wasm_locals = Vec::<Option<WASMLocalVariableRef>>::new();
        for _ in 0 .. arg_types.len() {
            wasm_locals.push(None);
        }

        // saving values already located on WASM stack to WASM locals
        for val_idx in 0 .. arg_types.len() {
            let loc_idx = arg_types.len() - val_idx - 1;
            let val = self.top(val_idx);
            if self.value_is_wasm_stack(&val) {
                let val_type = self.value_type(&val);
                let wasm_t = match val_type {
                    CircomValueType::WASM(wt) => wt,
                    CircomValueType::FR => WASMType::PTR,
                    CircomValueType::FRArray(..) => WASMType::PTR
                };

                // saving value from WASM stack to temporary local
                let tmp_local = self.func.borrow_mut().new_wasm_local(wasm_t);
                self.func.borrow_mut().gen_local_set(&tmp_local);
                wasm_locals[loc_idx] = Some(tmp_local);
            }
        }

        // identifying values with required int -> Fr conversion

        let mut requires_conversion_to_fr = Vec::<bool>::new();
        let mut fr_temp_values_count = 0;

        for val_idx in 0 .. arg_types.len() {
            let stack_idx = arg_types.len() - val_idx - 1;
            let val = self.top(stack_idx);

            let is_conv = match arg_types[val_idx] {
                CircomValueType::FR => {
                    match self.value_type(&val) {
                        CircomValueType::FR => false,
                        CircomValueType::WASM(..) => true,
                        CircomValueType::FRArray(..) => {
                            panic!("Fr arrays should not reach here");
                        }
                    }
                }
                CircomValueType::WASM(..) => false,
                CircomValueType::FRArray(..) => {
                    panic!("Fr arrays should not reach here");
                }
            };

            requires_conversion_to_fr.push(is_conv);

            if is_conv {
                fr_temp_values_count += 1;
            }

            if is_conv && val_idx == 0 && first_is_ptr {
                panic!("can't generate value conversion for pointer argument")
            }
        }

        // allocating temporary values for results of Fr conversion
        let fr_types: Vec<_> = std::iter::repeat([CircomValueType::FR])
            .flatten()
            .take(fr_temp_values_count)
            .collect();
        let fr_alloc_result = self.alloc_temp_n(&fr_types);

        // saving allocated fr values
        let mut fr_temp_vals = Vec::<Option<TemporaryStackValueRef>>::new();
        let mut fr_alloc_result_idx = 0;
        for val_idx in 0 .. arg_types.len() {
            if requires_conversion_to_fr[val_idx] {
                fr_temp_vals.push(Some(fr_alloc_result[fr_alloc_result_idx].clone()));
                fr_alloc_result_idx += 1;
            } else {
                fr_temp_vals.push(None);
            }
        }

        // loading values onto WASM stack and converting them to specified argument type
        for val_idx in 0 .. arg_types.len() {
            // NOTE: taking into account number of allocated temporary fr values when
            // calculating stack index
            let stack_idx = arg_types.len() + fr_temp_values_count - val_idx - 1;
            match &wasm_locals[val_idx] {
                Some(wasm_local) => {
                    let val = self.top(stack_idx);
                    match arg_types[val_idx] {
                        CircomValueType::FRArray(..) => {
                            panic!("Fr arrays should not reach here");
                        }
                        CircomValueType::FR => {
                            if self.value_type(&val).is_fr() {
                                self.func.borrow_mut().gen_local_get(&wasm_local);
                            } else {
                                let temp_fr_val = fr_temp_vals[val_idx].clone().unwrap();

                                // loading pointer to temporary fr value
                                self.load_temp_value_ptr_to_wasm_stack(&temp_fr_val);

                                // loading argument from WASM local
                                self.func.borrow_mut().gen_local_get(&wasm_local);

                                // converting argument to i64
                                self.func.borrow_mut().gen_i64_extend_i32_s();

                                // converting int to Fr value with fp256_set_ui function
                                let fp256_set_ui =
                                    self.module.borrow().ligetron().fp256_set_ui.to_wasm();
                                self.func.borrow_mut().gen_call(&fp256_set_ui);

                                // loading pointer to temporary fr value again
                                self.load_temp_value_ptr_to_wasm_stack(&temp_fr_val);
                            }
                        }
                        CircomValueType::WASM(wasm_comp_tp) => {
                            match self.value_type(&val) {
                                CircomValueType::FR => {
                                    panic!("Conversion from Fr to wasm is not supported yet");
                                }
                                CircomValueType::FRArray(..) => {
                                    panic!("Fr arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }

                                    // loading argument from WASM local
                                    self.func.borrow_mut().gen_local_get(&wasm_local);
                                }
                            }
                        }
                    }
                }
                None => {
                    let val = self.top(stack_idx);
                    let val_type = self.value_type(&val);

                    match arg_types[val_idx] {
                        CircomValueType::FRArray(..) => {
                            panic!("Fr arrays should not reach here");
                        }
                        CircomValueType::FR => {
                            match val_type {
                                CircomValueType::FRArray(..) => {
                                    panic!("Fr arrays should not reach here");
                                }
                                CircomValueType::FR => {
                                    // loading argument to WASM stack using generic load
                                    self.value_load_ptr_to_wasm_stack(&val);
                                }
                                CircomValueType::WASM(_wasm_type) => {
                                    let temp_fr_val = fr_temp_vals[val_idx].clone().unwrap();

                                    // loading pointer to temporary fr value
                                    self.load_temp_value_ptr_to_wasm_stack(&temp_fr_val);

                                    // loading argument to WASM stack
                                    self.value_load_int_to_wasm_stack(&val);

                                    // converting argument to i64
                                    self.func.borrow_mut().gen_i64_extend_i32_s();

                                    // converting int to Fr value with fp256_set_ui function
                                    let fp256_set_ui =
                                        self.module.borrow().ligetron().fp256_set_ui.to_wasm();
                                    self.func.borrow_mut().gen_call(&fp256_set_ui);

                                    // loading pointer to temporary fr value again
                                    self.load_temp_value_ptr_to_wasm_stack(&temp_fr_val);
                                }
                            }
                        }
                        CircomValueType::WASM(arg_wasm_type) => {
                            match val_type {
                                CircomValueType::FRArray(..) => {
                                    panic!("Fr arrays should not reach here");
                                }
                                CircomValueType::FR => {
                                    panic!("Conversion from Fr to wasm is not supported yet");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if arg_wasm_type != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }

                                    // loading parameter to WASM stack
                                    if val_idx == 0 && first_is_ptr {
                                        self.value_load_ptr_to_wasm_stack(&val);
                                    } else {
                                        self.value_load_int_to_wasm_stack(&val);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // generating operation
        op(self.func.borrow_mut());

        // TODO:
        // implement stack deallocation after conversion from wasm to fr is implemented

        // removing parameters and allocated Fr values from stack
        self.pop(arg_types.len() + fr_temp_values_count);
    }
    
    /// Loads specified number of top stack values onto WASM stack converting them to
    /// specified computation type
    pub fn load_values_to_wasm_stack(&mut self, count: usize, computation_tp: CircomValueType) {
        if count == 0 {
            return;
        }

        // We need to put all values onto WASM stack in correct order. For that,
        // we first store all values already located on WASM stack to temporary locals and
        // then load them again

        let mut values_locals = Vec::<Option<WASMLocalVariableRef>>::new();
        for _ in 0 .. count {
            values_locals.push(None);
        }

        // saving values already located on WASM stack to WASM locals 
        for val_idx in 0 .. count {
            let loc_idx = count - val_idx - 1;
            let val = self.top(val_idx);
            if self.value_is_wasm_stack(&val) {
                let val_type = self.value_type(&val);
                let wasm_t = match val_type {
                    CircomValueType::WASM(wt) => wt,
                    CircomValueType::FR => WASMType::PTR,
                    CircomValueType::FRArray(..) => WASMType::PTR
                };

                // saving value from WASM stack to temporary local
                let tmp_local = self.func.borrow_mut().new_wasm_local(wasm_t);
                self.func.borrow_mut().gen_local_set(&tmp_local);
                values_locals[loc_idx] = Some(tmp_local);
            }
        }

        // loading values onto WASM stack and converting them to specified computation type
        for val_idx in 0 .. count {
            let stack_idx = count - val_idx - 1;
            match &values_locals[val_idx] {
                Some(wasm_local) => {
                    self.func.borrow_mut().gen_local_get(&wasm_local);
                    let val = self.top(stack_idx);
                    match computation_tp {
                        CircomValueType::FRArray(..) => {
                            panic!("Fr arrays should not reach here");
                        }
                        CircomValueType::FR => {
                            if !self.value_type(&val).is_fr() {
                                panic!("Conversion from wasm to Fr is not supported yet");
                            }
                        }
                        CircomValueType::WASM(wasm_comp_tp) => {
                            match self.value_type(&val) {
                                CircomValueType::FR => {
                                    panic!("Conversion from Fr to wasm is not supported yet");
                                }
                                CircomValueType::FRArray(..) => {
                                    panic!("Fr arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }
                                }
                            }
                        }
                    }
                }
                None => {
                    // loading parameter onto WASM stack using generic load
                    self.value_load_ptr_to_wasm_stack(&self.top(stack_idx));

                    let val = self.top(stack_idx);
                    match computation_tp {
                        CircomValueType::FRArray(..) => {
                            panic!("Fr arrays should not reach here");
                        }
                        CircomValueType::FR => {
                            if !self.value_type(&val).is_fr() {
                                panic!("Conversion from wasm to Fr is not supported yet");
                            }
                        }
                        CircomValueType::WASM(wasm_comp_tp) => {
                            match self.value_type(&val) {
                                CircomValueType::FR => {
                                    panic!("Conversion from Fr to wasm is not supported yet");
                                }
                                CircomValueType::FRArray(..) => {
                                    panic!("Fr arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }
                                }
                            }
                        }
                    }
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

        self.load_values_to_wasm_stack(num_stack_vals, CircomValueType::FR);
 
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
    pub fn load_wasm_const(&mut self, tp: WASMType, val: i64) -> CircomStackValueRef {
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
                    self.load_ref(loc.clone());
                    let fp256_clear = &self.module.borrow().ligetron().fp256_clear.clone();
                    self.gen_call(&fp256_clear);
                }
                CircomValueType::FRArray(size) => {
                    for idx in 0 .. *size {
                        self.load_i32_const(idx as i32);
                        self.load_array_element_ref(loc.clone());
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
