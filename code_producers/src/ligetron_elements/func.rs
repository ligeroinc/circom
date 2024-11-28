
use super::fr::*;
use super::log::*;
use super::memory_stack::*;
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;

use std::rc::Rc;
use std::cell::{RefCell, RefMut};

use WASMType::*;


/// Represents current Circom function being generated
pub struct CircomFunction {
    /// Size of Circom variables, in 32bit
    size_32_bit: usize,

    /// FR context
    fr: FRContext,

    /// Reference to underlying WASM function for this Circom function
    func: Rc<RefCell<WASMFunction>>,

    /// Reference to stack frame in global memory for this function
    mem_frame_: Rc<RefCell<MemoryStackFrame>>,

    /// Circom stack frame
    frame: CircomStackFrame,
}

impl CircomFunction {
    /// Constructs new function generator
    pub fn new(size_32_bit: usize,
               fr: FRContext,
               module: Rc<RefCell<WASMModule>>,
               stack_ptr: WASMGlobalVariableRef,
               name: String,
               n_local_vars: usize) -> CircomFunction {

        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("CIRCOM FUNCTION BEGIN: {}", name);
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        // creating underlying WASM function
        let func = WASMFunction::new(module.clone(), name);

        // creating new memory stack frame for this funcion
        let mem_frame = Rc::new(RefCell::new(MemoryStackFrame::new(module.clone(),
                                                                   stack_ptr.clone(),
                                                                   func.inst_gen_rc().clone(),
                                                                   func.frame_rc().clone())));

        let func_rc = Rc::new(RefCell::new(func));

        let mut cfunc = CircomFunction {
            size_32_bit: size_32_bit,
            fr: fr,
            func: func_rc.clone(),
            mem_frame_: mem_frame.clone(),
            frame: CircomStackFrame::new(size_32_bit, func_rc, mem_frame),
        };

        // allocating circom local variables
        for i in 0 .. n_local_vars {
            let var_name = format!("local_var_{}", i);
            let _ = cfunc.frame.new_local_var(var_name, CircomValueType::FR);
        }

        return cfunc;
    }

    /// Returns reference to memory stack frame
    pub fn mem_frame(&self) -> RefMut<MemoryStackFrame> {
        return self.mem_frame_.as_ref().borrow_mut();
    }

    /// Returns reference to Circom stack frame
    pub fn get_frame(&mut self) -> &mut CircomStackFrame {
        return &mut self.frame;
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.func.borrow_mut().set_export_name(name);
    }

    /// Generates function code as list of instructions. Makes this instance invalid
    pub fn generate(&mut self, gen_func_entry_exit: bool) -> Vec<String> {
        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("CIRCOM FUNCTION END: {}", self.func.borrow().name());
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        // checking that stack is empty
        self.frame.check_empty();

        // generating function entry and exit code code for memory stack frame
        if gen_func_entry_exit {
            self.mem_frame().gen_func_entry();
            self.mem_frame().gen_func_exit();
        }

        return self.func.borrow_mut().generate();
    }

    /// Loads memory value at const address on sack
    pub fn load_mem_const(&mut self, tp: CircomValueType, addr: usize) {
        self.frame.push_mem_const(tp, addr);
    }

    /// Loads WASM constant value on stack
    pub fn load_const(&mut self, tp: WASMType, val: i64) {
        self.frame.load_const(tp, val);
    }


    ////////////////////////////////////////////////////////////
    /// Local variables

    /// Creates new local variable
    pub fn new_local_var(&mut self, name: String, tp: CircomValueType) -> CircomLocalVariableRef {
        return self.frame.new_local_var(name, tp);
    }

    /// Returns reference to local variable with specified index
    pub fn local_var(&self, idx: usize) -> CircomLocalVariableRef {
        return self.frame.local_var(idx);
    }

    /// Loads reference to local variable on stack
    pub fn load_local_var_ref(&mut self, var_ref: &CircomLocalVariableRef) {
        self.frame.load_local_var_ref(var_ref);
    }


    ////////////////////////////////////////////////////////////
    /// Function parameters

    /// Creates new function parameter
    pub fn new_param(&mut self, name: String, tp: CircomValueType) -> CircomParameterRef {
        return self.frame.new_param(name, tp);
    }

    /// Returns reference to parameter with specified index
    pub fn param(&self, idx: usize) -> CircomParameterRef {
        return self.frame.param(idx);
    }

    /// Loads reference to parameter on stack
    pub fn load_param_ref(&mut self, par_ref: &CircomParameterRef) {
        self.frame.load_param_ref(par_ref);
    }


    ////////////////////////////////////////////////////////////
    /// Function return values

    /// Creates new return value
    pub fn new_ret_val(&mut self, tp: CircomValueType) -> CircomReturnValueRef {
        return self.frame.new_ret_val(tp);
    }

    /// Returns reference to return value at specified index
    pub fn ret_val(&self, idx: usize) -> CircomReturnValueRef {
        return self.frame.ret_val(idx);
    }

    /// Loads reference to return value on stack
    pub fn load_ret_val_ref(&mut self, ret_val: &CircomReturnValueRef) {
        self.frame.load_ret_val_ref(ret_val);
    }
    

    /// Creates new function parameter

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_named_local(name, tp);
    }

    /// Adds Circom return value to function
    pub fn add_circom_ret_val(&mut self, name: &str) -> CircomValueRef {
        let ret_val = self.frame.new_ret_val(CircomValueType::FR);
        return CircomValueRef::MemoryRefWASMLocal(self.frame.ret_val_wasm_loc(&ret_val).clone());
    }

    /// Creates new Circom function parameter with specified name
    pub fn new_circom_param(&mut self, name: &str) -> CircomValueRef {
        let par = self.frame.new_param(name.to_string(), CircomValueType::FR);
        return CircomValueRef::MemoryRefWASMLocal(self.frame.param_wasm_loc(&par).clone());
    }

    /// Returns count of function parameters
    pub fn params_count(&self) -> usize {
        return self.frame.params_count();
    }

    /// Returns return value at specified index
    pub fn circom_ret_val(&self, idx: usize) -> CircomValueRef {
        // pointers for circom result values are passed as first parameters
        let par = self.func.borrow().param(idx);
        return CircomValueRef::MemoryRefWASMLocal(par);
    }


    ////////////////////////////////////////////////////////////
    // WASM Instructions generation

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.func.borrow_mut().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func.borrow_mut().gen_comment(str);
    }

    /// Generates call instruction
    pub fn gen_wasm_call(&mut self, func: &WASMFunctionRef) {
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

    /// Generates setting value of a local
    pub fn gen_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        self.func.borrow_mut().gen_local_set(var_ref);
    }

    /// Generates getting value of a global
    pub fn gen_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.func.borrow_mut().gen_global_get(var_ref);
    }

    /// Generates setting value of a global
    pub fn gen_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.func.borrow_mut().gen_global_set(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.func.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, tp: WASMType, value: i64) {
        self.func.borrow_mut().gen_const(tp, value);
    }

    /// Generates add instruction
    pub fn gen_add(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_add(tp);
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
    // Stack manipulation

    /// Allocates new value in memory stack frame. Returns reference to allocated value.
    pub fn alloc_mem_stack(&mut self, types: Vec<CircomValueType>) -> Vec<MemoryStackValueRef> {
        return self.frame.alloc_mem_stack(types);
    }

    /// Drops specified number of values located on top of stack
    pub fn drop(&mut self, count: usize) {
        self.frame.drop(count);
    }

    /// Loads value stored in WASM local on top of stack
    pub fn load_wasm_local(&mut self, loc: &WASMLocalVariableRef) {
        self.frame.push_wasm_local(loc);
    }

    /// Loads pointer to memory stack value on top of stack
    pub fn load_mem_stack_ptr(&mut self,
                              tp: CircomValueType,
                              val_ref: &MemoryStackValueRef) -> CircomStackValueRef {
        return self.frame.push_mem_stack_ptr(tp, val_ref);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Allocates Fr value on stack for result of operation
    pub fn alloc_fr_result(&mut self) {
        self.frame.alloc_mem_stack(vec![CircomValueType::FR]);
    }

    /// Generates Fr mul operation
    pub fn fr_mul(&mut self) {
        self.gen_call(&self.fr.mul.clone());
    }

    /// Generates Fr div operation
    pub fn fr_div(&mut self) {
        self.gen_call(&self.fr.div.clone());
    }

    /// Generates Fr add operation
    pub fn fr_add(&mut self) {
        self.gen_call(&self.fr.add.clone());
    }

    /// Generates Fr sub operation
    pub fn fr_sub(&mut self) {
        self.gen_call(&self.fr.sub.clone());
    }

    /// Generates Fr pow operation
    pub fn fr_pow(&mut self) {
        self.gen_call(&self.fr.pow.clone());
    }

    /// Generates Fr idiv operation
    pub fn fr_idiv(&mut self) {
        self.gen_call(&self.fr.idiv.clone());
    }

    /// Generates Fr mod operation
    pub fn fr_mod(&mut self) {
        self.gen_call(&self.fr.mod_.clone());
    }

    /// Generates Fr shl operation
    pub fn fr_shl(&mut self) {
        self.gen_call(&self.fr.shl.clone());
    }

    /// Generates Fr shr operation
    pub fn fr_shr(&mut self) {
        self.gen_call(&self.fr.shr.clone());
    }

    /// Generates Fr leq operation
    pub fn fr_leq(&mut self) {
        self.gen_call(&self.fr.leq.clone());
    }

    /// Generates Fr geq operation
    pub fn fr_geq(&mut self) {
        self.gen_call(&self.fr.geq.clone());
    }

    /// Generates Fr lt operation
    pub fn fr_lt(&mut self) {
        self.gen_call(&self.fr.lt.clone());
    }

    /// Generates Fr gt operation
    pub fn fr_gt(&mut self) {
        self.gen_call(&self.fr.gt.clone());
    }

    /// Generates Fr eq operation
    pub fn fr_eq(&mut self) {
        self.gen_call(&self.fr.eq.clone());
    }

    /// Generates Fr neq operation
    pub fn fr_neq(&mut self) {
        self.gen_call(&self.fr.neq.clone());
    }

    /// Generates Fr lor operation
    pub fn fr_lor(&mut self) {
        self.gen_call(&self.fr.lor.clone());
    }

    /// Generates Fr land operation
    pub fn fr_land(&mut self) {
        self.gen_call(&self.fr.land.clone());
    }

    /// Generates Fr bor operation
    pub fn fr_bor(&mut self) {
        self.gen_call(&self.fr.bor.clone());
    }

    /// Generates Fr band operation
    pub fn fr_band(&mut self) {
        self.gen_call(&self.fr.band.clone());
    }

    /// Generates Fr bxor operation
    pub fn fr_bxor(&mut self) {
        self.gen_call(&self.fr.bxor.clone());
    }

    /// Generates Fr neg operation
    pub fn fr_neg(&mut self) {
        self.gen_call(&self.fr.neg.clone());
    }

    /// Generates Fr lnot operation
    pub fn fr_lnot(&mut self) {
        self.gen_call(&self.fr.lnot.clone());
    }

    /// Generates Fr bnot operation
    pub fn fr_bnot(&mut self) {
        self.gen_call(&self.fr.bnot.clone());
    }


    /// Creates new Circom local variable with specified name
    pub fn new_circom_var(&mut self, name: &str) -> CircomValueRef {
        let var = self.frame.new_local_var(name.to_string(), CircomValueType::FR);
        return CircomValueRef::MemoryStackLocal(self.frame.local_var_mem_loc(&var).clone());
    }

    /// Returns reference Circom local variable with specified index
    pub fn circom_var(&self, idx: usize) -> CircomValueRef {
        let par_cnt = self.frame.params_count();
        if idx < par_cnt {
            let par = self.frame.param(idx);
            return CircomValueRef::MemoryRefWASMLocal(self.frame.param_wasm_loc(&par).clone());
        } else {
            let var = self.frame.local_var(idx - par_cnt);
            return CircomValueRef::MemoryStackLocal(self.frame.local_var_mem_loc(&var).clone());
        }
    }

    /// Generates saving Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_circom_store(&mut self) {
        // calling copy function
        self.gen_call(&self.fr.copy.clone());
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
                }
            }
        }

        if num_stack_vals != 0 {
            let mut stack_idx = num_stack_vals - 1;
            let mut wasm_stack_loaded = false;

            // loading pointers to FR return values to WASM stack
            for _ in func_ref.tp().ret_types().iter().filter(|t| t.is_fr()) {
                let stack_val = self.frame.top(stack_idx);
                let stack_val_type = self.frame.value_type(&stack_val);
                if stack_val_type != CircomValueType::FR {
                    panic!("Function call return types mismatch");
                }

                self.frame.gen_wasm_stack_load(stack_val);
                assert!(stack_idx > 0);
                stack_idx -= 1;
                wasm_stack_loaded = true;
            }

            // loading parameters to WASM stack
            for par in func_ref.tp().params() {
                let stack_val = self.frame.top(stack_idx);
                let stack_val_kind = self.frame.value_kind(&stack_val);
                // if *par != stack_val_tp {
                //     panic!("Function call parameter types mismatch: expected {}, found {}",
                //            par.to_string(), stack_val_tp.to_string());
                // }

                match stack_val_kind {
                    CircomStackValueKind::WASMSTack(..) => {
                        if wasm_stack_loaded {
                            // we can't duplicate values located in WASM stack
                            panic!("Duplicating WASM stack values is not supported");
                        }
                    },
                    _ => {
                        self.frame.gen_wasm_stack_load(stack_val);
                    }
                }

                stack_idx -= 1;
            }
        }

        // Generating call instruction
        self.func.borrow_mut().gen_call(&func_ref.to_wasm());

        // removing parameter values from stack
        self.frame.pop(func_ref.tp().params().len());

        // adding WASM return values to stack
        for ret_type in func_ref.tp().ret_types() {
            match ret_type {
                CircomValueType::WASM(wasm_type) => {
                    self.frame.push_wasm_stack(wasm_type.clone());
                },
                _ => {}
            }
        }
    }


    ////////////////////////////////////////////////////////////
    // Debugging

    /// Dumps state for debugging
    pub fn debug_dump_state(&self) {
        debug_log!("CIRCOM STACK:");
        debug_log!("{}", self.frame.dump());
        debug_log!("");

        debug_log!("MEMORY STACK:");
        debug_log!("{}", self.mem_frame_.borrow().dump());
        debug_log!("");

        debug_log!("WASM STACK:");
        debug_log!("{}", self.func.borrow().dump_stack());
        debug_log!("");
    }
}
