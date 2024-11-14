
use super::MemoryStackFrame;
use super::types::*;
use super::wasm::*;
use super::MemoryStackLocalRef;

use std::rc::Rc;
use std::cell::{RefCell, RefMut};

use WASMType::*;


/// Circom local variable
struct CircomLocalVariable {
    /// Name of variable
    name: String,

    /// Reference to local variable in memory stack frame
    mem_ref: MemoryStackLocalRef
}

impl CircomLocalVariable {
    /// Creates new local variable
    fn new(name: &str, mem_ref: MemoryStackLocalRef) -> CircomLocalVariable {
        return CircomLocalVariable {
            name: name.to_string(),
            mem_ref: mem_ref
        };
    }
}


/// Reference to Circlom local variable
#[derive(Clone)]
pub struct CircomLocalVariableRef {
    idx: usize
}

impl CircomLocalVariableRef {
    /// Creates new local variable reference
    fn new(idx: usize) -> CircomLocalVariableRef {
        return CircomLocalVariableRef {
            idx: idx
        };
    }
}


/// Represents current Circom function being generated
pub struct CircomFunction {
    /// Size of Circom variables, in 32bit
    size_32_bit: usize,

    /// Reference to underlying WASM function for this Circom function
    func: Rc<RefCell<WASMFunction>>,

    /// Reference to parent module being generaed
    module_: Rc<RefCell<Module>>,

    /// Reference to global variable for memory stack pointer
    stack_ptr: GlobalVariableRef,

    /// Reference to stack frame in global memory for this function
    mem_frame_: Rc<RefCell<MemoryStackFrame>>,

    /// Circom local variables
    vars: Vec<CircomLocalVariable>,

    /// Function export name
    export_name: Option<String>
}

impl CircomFunction {
    /// Constructs new function generator
    pub fn new(size_32_bit: usize,
               module: Rc<RefCell<Module>>,
               stack_ptr: GlobalVariableRef,
               name: String,
               params: &Vec<(String, ValueType)>,
               ret_types: Vec<ValueType>) -> CircomFunction {

        let mut wasm_params = Vec::<(String, WASMType)>::new();

        // adding WASM parameters for all specified parameters
        for (name, tp) in params {
            match tp {
                ValueType::WASM(wasm_type) => {
                    wasm_params.push((name.clone(), *wasm_type));
                },
                ValueType::FR => {
                    wasm_params.push((name.clone(), WASMType::PTR));
                }
            }
        }

        // processing return types, adding parameters for FR types and normal WASM return types
        // for WASM types
        let mut wasm_ret_types = Vec::<WASMType>::new();
        let mut ret_val_idx: usize = 0;
        for ret_type in ret_types {
            match ret_type {
                ValueType::WASM(wasm_type) => {
                    wasm_ret_types.push(wasm_type);
                },
                ValueType::FR => {
                    let par_name = format!("ret_val_{}", ret_val_idx);
                    wasm_params.push((par_name, WASMType::PTR));
                    ret_val_idx += 1;
                }
            }
        }

        // creating underlying WASM function
        let mut func = WASMFunction::new(module.clone(), name, &wasm_params, wasm_ret_types);

        // creating new memory stack frame for this funcion
        let mem_frame = Rc::new(RefCell::new(MemoryStackFrame::new(module.clone(),
                                                                   func.inst_gen_rc().clone(),
                                                                   func.frame_rc().clone())));

        return CircomFunction {
            size_32_bit: size_32_bit,
            func: Rc::new(RefCell::new(func)),
            module_: module.clone(),
            stack_ptr: stack_ptr,
            mem_frame_: mem_frame,
            vars: Vec::new(),
            export_name: None,
        }
    }

    /// Returns reference to memory stack frame
    pub fn mem_frame(&self) -> RefMut<MemoryStackFrame> {
        return self.mem_frame_.as_ref().borrow_mut();
    }

    /// Adds WASM return type for function
    pub fn add_wasm_ret_type(&mut self, tp: WASMType) {
        self.func.borrow_mut().add_ret_type(tp);
    }

    /// Adds new WASM named function parameter. Returns refence to function parameter.
    pub fn new_wasm_param(&mut self, name: &str, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_param(name, tp);
    }

    /// Returns reference to variable for parameter with specified index
    pub fn wasm_param(&self, idx: usize) -> WASMLocalVariableRef {
        return self.func.borrow().param(idx);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_named_local(name, tp);
    }

    /// Adds new unnamed WASM local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_wasm_local(tp);
    }

    // /// Returns reference to WASM local variable corresponding to reference
    // fn wasm_local(&self, lref: &WASMLocalVariableRef) -> &WASMLocalVariable {
    //     return self.wasm_frame().wasm_local(lref);
    // }

    /// Generates reference to WASM local
    fn gen_wasm_local_ref(&self, local: &WASMLocalVariableRef) -> String {
        return self.func.borrow().gen_local_ref(local);
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.export_name = Some(name.to_string());
    }

    /// Generates function code as list of instructions. Makes this instance invalid
    pub fn generate(&mut self) -> Vec<String> {
        return self.func.borrow_mut().generate();
    }


    ////////////////////////////////////////////////////////////
    // Instructions generation

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.func.borrow_mut().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func.borrow_mut().gen_comment(str);
    }

    /// Generates call instruction
    pub fn gen_call(&mut self, func: FunctionRef) {
        self.func.borrow_mut().gen_call(func);
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        self.func.borrow_mut().gen_local_get(var_ref);
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.func.borrow_mut().gen_local_set_expr(local, expr);
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        self.func.borrow_mut().gen_local_get(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.func.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, tp: WASMType, value: i64) {
        self.func.borrow_mut().gen_const(tp, value);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_mul(tp);
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_load(tp);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Adds Circom return type to function
    pub fn add_ret_val(&mut self) {
        for _ in 0 .. self.size_32_bit {
            self.add_wasm_ret_type(I32);
        }
    }

    /// Creates new Circom function parameter with specified name
    pub fn new_param(&mut self, name: &str) -> CircomLocalVariableRef {
        // creating WASM parameters variables for representing Circom parameter
        let mut vars = Vec::<WASMLocalVariableRef>::new();
        for i in 0 .. self.size_32_bit {
            vars.push(self.new_wasm_param(&format!("{}_{}", name, i), I32));
        }

        let var_idx = self.vars.len();

        // creating Circom variable
        let var = CircomLocalVariable::new(name, vars);
        self.vars.push(var);

        return CircomLocalVariableRef::new(var_idx);
    }

    /// Creates new Circom local variable with specified name
    pub fn new_var(&mut self, name: &str) -> CircomLocalVariableRef {
        // allocating space for variable in memory stack frame
        let mem_loc_ref = self.mem_frame().new_local(self.size_32_bit);

        let var_idx = self.vars.len();

        // creating Circom variable
        let var = CircomLocalVariable::new(name, mem_loc_ref);
        self.vars.push(var);

        return CircomLocalVariableRef::new(var_idx);
    }

    /// Retrns Circom variable corresponding to variable reference
    fn get_var(&self, var_ref: &CircomLocalVariableRef) -> &CircomLocalVariable {
        return &self.vars[var_ref.idx];
    }

    /// Generates loading of variable to wasm stack
    pub fn gen_load_var(&mut self, var_ref: &CircomLocalVariableRef) {
        let var = &self.get_var(var_ref);
        for loc in var.locals.clone() {
            self.gen_local_get(&loc);
        }
    }

    /// Generates saving wasm stack value to variable
    pub fn gen_store_var(&mut self, var_ref: &CircomLocalVariableRef) {
        let var = &self.get_var(var_ref);
        for loc in var.locals.clone().iter().rev() {
            self.gen_local_set(&loc);
        }
    }
}
