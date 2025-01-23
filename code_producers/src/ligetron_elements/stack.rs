
use super::arrays::*;
use super::constants::ConstValue;
use super::memory_stack::*;
use super::module::*;
use super::structs::*;
use super::temp::*;
use super::types::*;
use super::value::*;
use super::wasm::*;
use super::wasm_stack::*;
use super::CircomComponent;

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

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        let mem_loc = frame.local_var_mem_loc(&self);
        frame.mem_frame.borrow_mut().gen_load_local_addr_igen(&mem_loc, 0, inst_gen);
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

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        let ptr_wasm_loc = frame.param_wasm_loc(self);
        inst_gen.gen_local_get(&ptr_wasm_loc);
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

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return false;
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        let ptr_wasm_loc = frame.ret_val_wasm_loc(self);
        inst_gen.gen_local_get(&ptr_wasm_loc);
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
    /// Reference to another value
    ValueRef(Box<dyn CircomValueRef>)
}


/// Value located in Circom stack frame
struct CircomStackValue {
    /// Actual value
    value: Box<dyn CircomValueRef>
}

impl CircomStackValue {
    /// Creates new stack value
    fn new(value: Box<dyn CircomValueRef>) -> CircomStackValue {
        return CircomStackValue {
            value: value
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
    pub mem_frame: Rc<RefCell<MemoryStackFrame>>,

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
    pub fn value_kind(&self, val_ref: &CircomStackValueRef) -> Box<dyn CircomValueRef> {
        return val_ref.value_rc.borrow().value.clone();
    }

    /// Returns true if value is located in WASM stack
    pub fn value_is_wasm_stack(&self, val_ref: &CircomStackValueRef) -> bool {
        return val_ref.value_rc.borrow().value.is_wasm_stack();
    }

    /// Returns true if value is located in memory stack
    pub fn value_is_mem_stack(&self, val_ref: &CircomStackValueRef) -> bool {
        return val_ref.value_rc.borrow().value.is_mem_stack();
    }

    /// Returns stack value type
    pub fn value_type(&self, val_ref: &CircomStackValueRef) -> CircomValueType {
        return val_ref.value_rc.borrow().value.xtype(self);
    }

    /// Performs additional actions for deallocating value
    fn value_deallocate(&mut self, val_ref: &CircomStackValueRef) {
        val_ref.value_rc.borrow().value.deallocate(&mut self.func.borrow().inst_gen_mut(), self);
    }

    /// Loads int value to WASM stack
    pub fn value_load_int_to_wasm_stack(&mut self, val_ref: &CircomStackValueRef) {
        val_ref.value_rc.borrow().value.gen_load_to_wasm_stack(&mut self.func.borrow().inst_gen_mut(), self);
    }

    /// Loads value pointer onto WASM stack
    pub fn value_load_ptr_to_wasm_stack(&mut self, val_ref: &CircomStackValueRef) {
        val_ref.value_rc.borrow().value.gen_load_ptr_to_wasm_stack(&mut self.func.borrow().inst_gen_mut(), self);
    }

    /// Dumps stack value to string
    fn value_dump(&self, val_ref: &CircomStackValueRef) -> String {
        return val_ref.value_rc.borrow().value.dump(self);
    }


    ////////////////////////////////////////////////////////////
    /// Stack manipulation

    /// Returns stack size
    fn size(&self) -> usize {
        return self.values.len();
    }

    /// Pushes value to stack
    fn push(&mut self, val: Box<dyn CircomValueRef>) -> CircomStackValueRef {
        let value = CircomStackValue::new(val);
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

    /// Returns total size of parameters
    pub fn params_size(&self) -> usize {
        let mut res = 0;
        for par in &self.params {
            let par_size = match &par.type_ {
                CircomValueType::FR => 1,
                CircomValueType::Array(tp, size) => {
                    match tp.as_ref() {
                        CircomValueType::FR => {
                            *size
                        }
                        _ => {
                            panic!("Non Fr parameters are not supported");
                        }
                    }
                }
                _ => {
                    panic!("Non Fr parameters are not supported");
                }
            };

            res += par_size;
        }

        return res;
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
            let temp_val = TemporaryStackValue::new(types[idx].clone(), mem_val.clone());

            // allocating Fr values
            self.alloc_fr_values(&mut self.func.borrow().inst_gen_mut(), &temp_val);

            // adding value onto logical stack
            self.push(Box::new(temp_val));

            // adding value reference to result list
            vals.push(TemporaryStackValueRef::new(types[idx].clone(), mem_val.clone()));
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
        val.gen_load_ptr_to_wasm_stack(&mut self.func.borrow().inst_gen_mut(), self);
    }


    ////////////////////////////////////////////////////////////
    /// References

    /// Loads reference to value on stack
    pub fn load_ref(&mut self, val: Box<dyn CircomValueRef>) {
        self.push(val);
    }

    /// Loads reference to subarray of array to stack
    pub fn load_array_slice_ref(&mut self, arr: Box<dyn CircomValueRef>, size: usize) {
        // checking parameter type
        match arr.xtype(self) {
            CircomValueType::Array(..) => {
            }
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        let val = &self.top(0).value_rc.borrow().value.clone();
        if let Some(const_val) = val.as_i32_const() {
            self.pop(1);
            let slice = array_slice(arr, const_val.value() as usize, size);
            self.push(slice);
        } else if let Some(index_val) = val.as_array_index() {
            let inst_gen_rc = self.func.borrow().inst_gen_rc().clone();
            gen_load_array_element_ptr_to_wasm_stack_dyn(self,
                                                         arr.as_ref(),
                                                         index_val.offset() as usize,
                                                         &mut inst_gen_rc.borrow_mut());

            self.pop(1);
            self.push(Box::new(WASMStackAddress::new(CircomValueType::FR)));
        } else {
            panic!("top stack value is not an index");
        }
    }

    /// Loads reference to element of array to stack
    pub fn load_array_element_ref(&mut self,
                                  arr: Box<dyn CircomValueRef>,
                                  val_type: CircomValueType) {
        // checking array variable type
        match arr.xtype(self) {
            CircomValueType::Array(..) => {},
            _ => {
                panic!("Can't get reference to local variable subarray of non array value");
            }
        }

        // calculating value address
        let val = &self.top(0).value_rc.borrow().value.clone();
        if let Some(const_val) = val.as_i32_const() {
            self.pop(1);
            let elt_ref = array_element(arr, const_val.value() as usize);
            self.push(elt_ref);
        } else if let Some(index_val) = val.as_array_index() {
            let inst_gen_rc = self.func.borrow().inst_gen_rc().clone();
            gen_load_array_element_ptr_to_wasm_stack_dyn(self,
                                                         arr.as_ref(),
                                                         index_val.offset() as usize,
                                                         &mut inst_gen_rc.borrow_mut());
            self.pop(1);
            self.push(Box::new(WASMStackAddress::new(val_type)));
        } else {
            panic!("top stack value is not an index");
        }
    }


    ////////////////////////////////////////////////////////////
    /// Indexes and Addresses

    /// Loads i32 const value on stack
    pub fn load_i32_const(&mut self, val: i32) {
        self.push(Box::new(ConstValue::new(val)));
    }

    /// Converts current stack top value to index. The top of stack must be a WASM I32 value
    pub fn convert_index(&mut self) {
        // loading value as integer to WASM stack
        self.value_load_int_to_wasm_stack(&self.top(0));

        // converting stack value
        self.pop(1);
        self.push(Box::new(ArrayIndex::new(0)));
    }

    /// Generates index add operation
    pub fn index_add(&mut self) {
        let first = self.top(0);
        let second = self.top(1);

        let first_val = first.value_rc.borrow().value.clone();
        let second_val = second.value_rc.borrow().value.clone();

        let res: Box<dyn CircomValueRef> = if let Some(first_const) = first_val.as_i32_const() {
            if let Some(second_const) = second_val.as_i32_const() {
                Box::new(ConstValue::new(first_const.value() + second_const.value()))
            } else if let Some(second_idx) = second_val.as_array_index() {
                Box::new(ArrayIndex::new(first_const.value() + second_idx.offset()))
            } else {
                panic!("don't know how to add indexes");
            }
        } else if let Some(first_idx) = first_val.as_array_index() {
            if let Some(second_const) = second_val.as_i32_const() {
                Box::new(ArrayIndex::new(first_idx.offset() + second_const.value()))
            } else if let Some(second_idx) = second_val.as_array_index() {
                self.func.borrow_mut().gen_add(WASMType::I32);
                Box::new(ArrayIndex::new(first_idx.offset() + second_idx.offset()))
            } else {
                panic!("don't know how to add indexes");
            }
        } else {
            panic!("don't know how to add indexes");
        };

        self.pop(2);
        self.push(res);
    }

    /// Generates index mul operation
    pub fn index_mul(&mut self) {
        let first = self.top(0);
        let second = self.top(1);

        let first_val = first.value_rc.borrow().value.clone();
        let second_val = second.value_rc.borrow().value.clone();

        let res: Box<dyn CircomValueRef> =  if let Some(first_const) = first_val.as_i32_const() {
            if let Some(second_const) = second_val.as_i32_const() {
                Box::new(ConstValue::new(first_const.value() * second_const.value()))
            } else if let Some(second_idx) = second_val.as_array_index() {
                self.func.borrow_mut().gen_const(WASMType::I32, first_const.value() as i64);
                self.func.borrow_mut().gen_mul(WASMType::I32);
                Box::new(ArrayIndex::new(first_const.value() * second_idx.offset()))
            } else {
                panic!("don't know how to add indexes");
            }
        } else if let Some(first_idx) = first_val.as_array_index() {
            if let Some(second_const) = second_val.as_i32_const() {
                self.func.borrow_mut().gen_const(WASMType::I32, second_const.value() as i64);
                self.func.borrow_mut().gen_mul(WASMType::I32);
                Box::new(ArrayIndex::new(first_idx.offset() * second_const.value()))
            } else if let Some(_second_idx) = second_val.as_array_index() {
                // do we need this case?
                panic!("NYI");
            } else {
                panic!("don't know how to add indexes");
            }
        } else {
            panic!("don't know how to add indexes");
        };

        self.pop(2);
        self.push(res);
    }

    /// Returns true if index located on top of stack is const
    pub fn top_index_is_const(&self, idx: usize) -> bool {
        return self.top(idx).value_rc.borrow().value.as_i32_const().is_some();
    }

    /// Returns constant offset of index located on top of stack
    pub fn top_index_offset(&self, idx: usize) -> i32 {
        let val = &self.top(idx).value_rc.borrow().value.clone();
        if let Some(const_val) = val.as_i32_const() {
            const_val.value()
        } else if let Some(index_val) = val.as_array_index() {
            index_val.offset()
        } else {
            panic!("top stack value is not and index value");
        }
    }

    /// Returns top stack value as component
    pub fn top_component(&self, top_idx: usize) -> Option<CircomComponent> {
        return match self.top(top_idx).value_rc.borrow().value.as_component() {
            Some(comp) => {
                Some(comp.clone())
            },
            None => None
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
                    CircomValueType::Array(..) => WASMType::PTR,
                    CircomValueType::Struct(..) => WASMType::PTR
                };

                // calculating actual value that is depended from WASM stack value
                let val_ref = self.value_kind(&val);
                if val_ref.xtype(self).is_fr() || (val_idx == 0 && first_is_ptr) {
                    val_ref.gen_load_ptr_to_wasm_stack(&mut self.func.borrow().inst_gen_mut(), self);
                } else {
                    val_ref.gen_load_to_wasm_stack(&mut self.func.borrow().inst_gen_mut(), self);
                }

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
                        CircomValueType::Array(tp, _size) => {
                            match tp.as_ref() {
                                CircomValueType::FR => false,
                                _ => {
                                    panic!("don't know how to convert non Fr array to Fr type");
                                }
                            }
                        }
                        CircomValueType::Struct(..) => {
                            panic!("don't know how to convert struct for Fr type");
                        }
                    }
                }
                CircomValueType::WASM(..) => false,
                CircomValueType::Array(..) => {
                    panic!("arrays should not reach here");
                }
                CircomValueType::Struct(..) => false,
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
                        CircomValueType::Array(..) => {
                            panic!("arrays should not reach here");
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
                                    // loading Fr address from WASM local
                                    self.func.borrow_mut().gen_local_get(&wasm_local);

                                    // converting Fr value to i64
                                    let fp256_get_ui =
                                        self.module.borrow().ligetron().fp256_get_ui.to_wasm();
                                    self.func.borrow_mut().gen_call(&fp256_get_ui);

                                    // converting i64 value to i32
                                    self.func.borrow_mut().gen_i32_wrap_i64();
                                }
                                CircomValueType::Array(..) => {
                                    panic!("arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }

                                    // loading argument from WASM local
                                    self.func.borrow_mut().gen_local_get(&wasm_local);
                                }
                                CircomValueType::Struct(..) => {
                                    panic!("don't know how to convert struct to WASM type");
                                }
                            }
                        }
                        CircomValueType::Struct(..) => {
                            match &self.value_type(&val) {
                                CircomValueType::Struct(..) => {
                                    // loading struct address from WASM local
                                    self.func.borrow_mut().gen_local_get(&wasm_local);
                                }
                                _ => {
                                    panic!("don't know how to convert value to struct")
                                }
                            }
                        }
                    }
                }
                None => {
                    let val = self.top(stack_idx);
                    let val_type = self.value_type(&val);

                    match arg_types[val_idx] {
                        CircomValueType::Array(..) => {
                            panic!("arrays should not reach here");
                        }
                        CircomValueType::FR => {
                            match val_type {
                                CircomValueType::Array(tp, _size) => {
                                    match tp.as_ref() {
                                        CircomValueType::FR => {
                                            // loading argument to WASM stack using generic load
                                            self.value_load_ptr_to_wasm_stack(&val);
                                        }
                                        _ => {
                                            panic!("should not reach here");
                                        }
                                    }
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
                                CircomValueType::Struct(..) => {
                                    panic!("don't know how to convert struct to Fr type");
                                }
                            }
                        }
                        CircomValueType::WASM(arg_wasm_type) => {
                            match val_type {
                                CircomValueType::Array(..) => {
                                    panic!("arrays should not reach here");
                                }
                                CircomValueType::FR => {
                                    // loading Fr pointer to WASM stack
                                    self.value_load_ptr_to_wasm_stack(&val);

                                    // converting Fr value to i64
                                    let fp256_get_ui =
                                        self.module.borrow().ligetron().fp256_get_ui.to_wasm();
                                    self.func.borrow_mut().gen_call(&fp256_get_ui);

                                    // converting i64 value to i32
                                    self.func.borrow_mut().gen_i32_wrap_i64();
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
                                CircomValueType::Struct(..) => {
                                    panic!("don't know how to convert struct to WASM type");
                                }
                            }
                        }
                        CircomValueType::Struct(..) => {
                            match &self.value_type(&val) {
                                CircomValueType::Struct(..) => {
                                    // loading struct address to WASM stack
                                    self.value_load_ptr_to_wasm_stack(&val);
                                }
                                _ => {
                                    panic!("don't know how to convert value to struct")
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
                    CircomValueType::Array(..) => WASMType::PTR,
                    CircomValueType::Struct(..) => WASMType::PTR
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
                        CircomValueType::Array(..) => {
                            panic!("arrays should not reach here");
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
                                CircomValueType::Array(..) => {
                                    panic!("arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }
                                }
                                CircomValueType::Struct(..) => {
                                    panic!("Call parameter can't be a struct");
                                }
                            }
                        }
                        CircomValueType::Struct(..) => {
                            panic!("Call parameter can't be a struct");
                        }
                    }
                }
                None => {
                    // loading parameter onto WASM stack using generic load
                    self.value_load_ptr_to_wasm_stack(&self.top(stack_idx));

                    let val = self.top(stack_idx);
                    match computation_tp {
                        CircomValueType::Array(..) => {
                            panic!("arrays should not reach here");
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
                                CircomValueType::Array(..) => {
                                    panic!("arrays should not reach here");
                                }
                                CircomValueType::WASM(val_wasm_tp) => {
                                    if wasm_comp_tp != val_wasm_tp {
                                        panic!("Conversion between wasm types is not implemented yet");
                                    }
                                }
                                CircomValueType::Struct(..) => {
                                    panic!("Call parameter can't be a struct");
                                }
                            }
                        }
                        CircomValueType::Struct(..) => {
                            panic!("Call parameter can't be a struct");
                        }
                    }
                }
            }
        }
    }

    /// Generates call to function passing values located on top of stack as parameters
    pub fn gen_call(&mut self, func_ref: &CircomFunctionRef) {

        // calculating argument types for function call
        
        let mut arg_types = Vec::<CircomValueType>::new();
        for ret_type in func_ref.tp().ret_types().iter().rev() {
            match ret_type {
                CircomValueType::WASM(_) => {
                    // WASM return values don't require parameter
                },
                CircomValueType::FR => {
                    arg_types.push(ret_type.clone());
                },
                CircomValueType::Array(..) => {
                    arg_types.push(ret_type.clone());
                }
                CircomValueType::Struct(..) => {
                    panic!("Call return value can't be a struct");
                }
            }
        }

        arg_types.append(&mut func_ref.tp().params().clone());

        // generating call using gen_op function
        self.gen_op(arg_types, false, |mut func| {
            func.gen_call(&func_ref.to_wasm());
        });

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
        return self.push(Box::new(WASMStackValue::new(tp)));
    }


    ////////////////////////////////////////////////////////////
    /// WASM stack manipulation

    /// Pushes value stored in WASM stack
    pub fn push_wasm_stack(&mut self, tp: WASMType) -> CircomStackValueRef {
        return self.push(Box::new(WASMStackValue::new(tp)));
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

    /// Allocates Fr (fp256) values for referenced object
    fn alloc_fr_values(&self, inst_gen: &mut InstructionGenerator, val: &dyn CircomValueRef) {
        match val.xtype(self) {
            CircomValueType::FR => {
                val.gen_load_ptr_to_wasm_stack(inst_gen, self);
                inst_gen.gen_const(WASMType::I32, 0);
                inst_gen.gen_store(&WASMType::I32);
                val.gen_load_ptr_to_wasm_stack(inst_gen, self);
                inst_gen.gen_call(&self.module.borrow().ligetron().fp256_init.to_wasm());
            }
            CircomValueType::Array(tp, size) => {
                for idx in 0 .. size {
                    self.alloc_fr_values(inst_gen, &ArrayElementRef::new(val.clone_ref(), idx));
                }
            }
            CircomValueType::WASM(_) => {
                // nothing to do for WASM types
            }
            CircomValueType::Struct(str) => {
                for idx in 0 .. str.fields.len() {
                    self.alloc_fr_values(inst_gen, &StructFieldRef::new(val.clone_ref(), idx));
                }
            }
        }
    }

    /// Generates function entry code
    pub fn gen_func_entry(&mut self) {
        let stack_rc = Rc::new(RefCell::new(WASMStackState::new()));
        let mut igen = InstructionGenerator::new(self.module.borrow().wasm_module_rc().clone(),
                                                 self.func.borrow().frame_rc().clone(),
                                                 stack_rc);

        igen.gen_comment("initializing Fr local variables");
        for loc in self.locals() {
            self.alloc_fr_values(&mut igen, &loc);
        }

        self.func.borrow_mut().inst_gen_mut().insert_insts_begin(&igen);


        self.mem_frame.borrow_mut().gen_func_entry();
    }

    /// deallocates Fr values for referenced object
    pub fn clear_fr_values(&self, inst_gen: &mut InstructionGenerator, val: &dyn CircomValueRef) {
        match val.xtype(self) {
            CircomValueType::FR => {
                val.gen_load_ptr_to_wasm_stack(inst_gen, self);
                inst_gen.gen_call(&self.module.borrow().ligetron().fp256_clear.to_wasm());
            }
            CircomValueType::Array(_tp, size) => {
                for idx in 0 .. size {
                    self.clear_fr_values(inst_gen, &ArrayElementRef::new(val.clone_ref(), idx));
                }
            }
            CircomValueType::WASM(_) => {
                // nothing to do for WASM types
            }
            CircomValueType::Struct(str) => {
                for idx in 0 .. str.fields.len() {
                    self.clear_fr_values(inst_gen, &StructFieldRef::new(val.clone_ref(), idx));
                }
            }
        }
    }

    /// Generates function exit code
    pub fn gen_func_exit(&mut self) {
        self.func.borrow_mut().gen_comment("deinitializing Fr local variables");
        for loc in self.locals() {
            let inst_gen_rc = self.func.borrow().inst_gen_rc().clone();
            self.clear_fr_values(&mut inst_gen_rc.borrow_mut(), &loc);
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
