
use super::fr::*;
use super::log::*;
use super::memory_stack::*;
use super::stack::*;
use super::types::*;
use super::wasm::*;
use super::CircomModule;

use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};

use WASMType::*;


/// Represents current Circom function being generated
pub struct CircomFunction {
    /// Reference to parent Circom module
    module: Rc<RefCell<CircomModule>>,

    /// Reference to underlying WASM function for this Circom function
    func: Rc<RefCell<WASMFunction>>,

    /// Reference to stack frame in global memory for this function
    mem_frame_: Rc<RefCell<MemoryStackFrame>>,

    /// Circom stack frame
    frame: CircomStackFrame,

    /// Reference to local variable containing array of FR values for all Circom
    /// local variables
    circom_locals_: CircomLocalVariableRef,

    /// Should constant values be generated as address values instead of FR values
    const_addr_mode: bool,
}

impl CircomFunction {
    /// Constructs new function generator
    pub fn new(module: Rc<RefCell<CircomModule>>,
               name: String,
               n_local_vars: usize) -> CircomFunction {

        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("CIRCOM FUNCTION BEGIN: {}", name);
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        // creating underlying WASM function
        let func = WASMFunction::new(module.borrow_mut().wasm_module_rc().clone(), name);

        // creating new memory stack frame for this funcion
        let mem_frame = Rc::new(RefCell::new(MemoryStackFrame::new(module.borrow().wasm_module_rc().clone(),
                                                                   module.borrow().stack_ptr().clone(),
                                                                   func.inst_gen_rc().clone(),
                                                                   func.frame_rc().clone())));

        let func_rc = Rc::new(RefCell::new(func));

        let mut frame = CircomStackFrame::new(module.borrow().var_size_32_bit(),
                                              func_rc.clone(),
                                              mem_frame.clone());

        // Allocating array of variables for Circom local variables. This is required
        // because Circom compiler references all variables by index, and we can't detect
        // real variable types (arrays vs values).
        let circom_locals = frame.new_local_var(format!("circom_local_vars"),
                                                CircomValueType::FRArray(n_local_vars));

        return CircomFunction {
            module: module,
            func: func_rc.clone(),
            mem_frame_: mem_frame.clone(),
            frame: frame,
            circom_locals_: circom_locals,
            const_addr_mode: false
        };
    }

    /// Returns reference to parent Circom module
    pub fn module_ref(&self) -> RefMut<CircomModule> {
        return self.module.as_ref().borrow_mut();
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

    /// Generates function code as list of instructions and appends them into module.
    /// Makes this instance invalid.
    pub fn generate(&mut self, gen_func_entry_exit: bool) {
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

        self.module_ref().append_instruction(&mut vec![format!("")]);
        self.module_ref().append_instruction(&mut self.func.borrow_mut().generate());
    }

    /// Sets constant address mode flag. Returns previous flag value.
    pub fn set_const_addr_mode(&mut self, val: bool) -> bool {
        let res = self.const_addr_mode;
        self.const_addr_mode = val;
        return res;
    }

    /// Loads memory value at const address on sack
    pub fn load_mem_const(&mut self, tp: CircomValueType, addr: usize) {
        self.frame.load_mem_const(tp, addr);
    }

    /// Loads WASM constant value on stack
    pub fn load_const(&mut self, tp: WASMType, val: i64) {
        self.frame.load_const(tp, val);
    }

    /// Loads U32 constant value on stack
    pub fn load_const_u32(&mut self, val: usize) {
        if self.const_addr_mode {
            self.load_const_index(val as i32);
        } else {
            self.load_const(WASMType::I64, val as i64);
            let raw_copy = self.module_ref().fr().raw_copy.clone();
            self.gen_call(&raw_copy);
        }
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

    /// Returns local variable type
    pub fn local_var_type(&self, var: &CircomLocalVariableRef) -> CircomValueType {
        return self.frame.local_var_type(var).clone();
    }

    /// Returns reference to local variable containing array for all
    /// preallocated Circom local variables
    pub fn circom_locals(&self) -> CircomLocalVariableRef {
        return self.circom_locals_.clone();
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

    /// Returns parameter type
    pub fn param_type(&self, par: &CircomParameterRef) -> CircomValueType {
        return self.frame.param_type(par).clone();
    }


    ////////////////////////////////////////////////////////////
    /// Function return values

    /// Creates new return value
    pub fn new_ret_val(&mut self, tp: CircomValueType, name: Option<String>) -> CircomReturnValueRef {
        return self.frame.new_ret_val(tp, name);
    }

    /// Returns reference to return value at specified index
    pub fn ret_val(&self, idx: usize) -> CircomReturnValueRef {
        return self.frame.ret_val(idx);
    }

    /// Returns return value type
    pub fn ret_val_type(&self, ret_val: &CircomReturnValueRef) -> CircomValueType {
        return self.frame.ret_val_type(ret_val).clone();
    }


    ////////////////////////////////////////////////////////////
    /// Temporary stack values

    /// Allocates temporary stack values
    pub fn alloc_temp_n(&mut self, types: &Vec<CircomValueType>) -> Vec<TemporaryStackValueRef> {
        return self.frame.alloc_temp_n(types);
    }

    /// Allocates single temporary stack value
    pub fn alloc_temp(&mut self, tp: CircomValueType) -> TemporaryStackValueRef {
        return self.frame.alloc_temp(&tp);
    }


    ////////////////////////////////////////////////////////////
    /// References

    /// Loads reference to value on stack
    pub fn load_ref<T: ConvertibleToValueRef>(&mut self, val: &T) {
        self.frame.load_ref(val);
    }

    /// Loads reference to subarray of array to stack
    pub fn load_array_slice_ref<T: ConvertibleToValueRef>(&mut self, arr: &T, size: usize) {
        self.frame.load_array_slice_ref(arr, size);
    }

    /// Loads reference to element of array to stack using top stack address value as offset
    pub fn load_array_element_ref<T: ConvertibleToValueRef>(&mut self, arr: &T) {
        self.frame.load_array_element_ref(arr);
    }


    ////////////////////////////////////////////////////////////
    /// Addresses and indexes

    /// Loads const address on stack
    pub fn load_const_index(&mut self, idx: i32) {
        self.frame.load_const_index(idx);
    }

    /// Converts current stack top value to address. The top of stack must be a WASM PTR value
    pub fn convert_index(&mut self) {
        self.frame.convert_index();
    }

    /// Generates address add operation
    pub fn index_add(&mut self) {
        self.frame.index_add();
    }

    /// Generates address mul operation
    pub fn index_mul(&mut self) {
        self.frame.index_mul();
    }

    /// Returns true if index located on top of stack is const index
    pub fn top_index_is_const(&self) -> bool {
        return self.frame.top_index_is_const();
    }

    /// Returns constant offset in index located on top of stack
    pub fn top_index_offset(&self) -> i32 {
        return self.frame.top_index_offset();
    }


    ////////////////////////////////////////////////////////////


    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_named_local(name, tp);
    }

    /// Creates new Circom function parameter with specified name
    pub fn new_circom_param(&mut self, name: &str) {
        self.frame.new_param(name.to_string(), CircomValueType::FR);
    }

    /// Returns count of function parameters
    pub fn params_count(&self) -> usize {
        return self.frame.params_count();
    }


    ////////////////////////////////////////////////////////////
    // WASM Instructions generation

    /// Generates empty code line
    pub fn gen_wasm_empty(&mut self) {
        self.func.borrow_mut().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_wasm_comment(&mut self, str: &str) {
        self.func.borrow_mut().gen_comment(str);
    }

    /// Generates call instruction
    pub fn gen_wasm_call(&mut self, func: &WASMFunctionRef) {
        self.frame.pop(func.tp().params().len());

        self.func.borrow_mut().gen_call(func);

        for ret_type in func.tp().ret_types() {
            self.frame.push_wasm_stack(ret_type.clone());
        }
    }

    /// Generates getting value of a local
    pub fn gen_wasm_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        let (_, tp) = self.func.borrow().local(var_ref);

        self.func.borrow_mut().gen_local_get(var_ref);
        self.frame.push_wasm_stack(tp);
    }

    /// Generates setting value of a local
    pub fn gen_wasm_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        let (_, tp) = self.func.borrow().local(var_ref);

        self.frame.pop_wasm_stack(&tp);
        self.func.borrow_mut().gen_local_set(var_ref);
    }

    /// Generates getting value of a global
    pub fn gen_wasm_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (_, tp) = self.module.borrow().wasm_module().get_global_ref_and_type(var_ref);

        self.func.borrow_mut().gen_global_get(var_ref);
        self.frame.push_wasm_stack(tp);
    }

    /// Generates setting value of a global
    pub fn gen_wasm_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (_, tp) = self.module.borrow().wasm_module().get_global_ref_and_type(var_ref);

        self.frame.pop_wasm_stack(&tp);
        self.func.borrow_mut().gen_global_set(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_wasm_drop(&mut self) {
        self.frame.pop(1);
        self.func.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_wasm_const(&mut self, tp: WASMType, value: i64) {
        self.func.borrow_mut().gen_const(tp.clone(), value);
        self.frame.push_wasm_stack(tp);
    }

    /// Generates add instruction
    pub fn gen_wasm_add(&mut self, tp: WASMType) {
        if tp == WASMType::PTR {
            self.frame.pop_wasm_ptr_ops();
        } else {
            self.frame.pop_wasm_stack(&tp);
            self.frame.pop_wasm_stack(&tp);
        }

        self.func.borrow_mut().gen_add(tp);

        self.frame.push_wasm_stack(tp);
    }

    /// Generates sub instruction
    pub fn gen_wasm_sub(&mut self, tp: WASMType) {
        if tp == WASMType::PTR {
            self.frame.pop_wasm_ptr_ops();
        } else {
            self.frame.pop_wasm_stack(&tp);
            self.frame.pop_wasm_stack(&tp);
        }

        self.func.borrow_mut().gen_sub(tp);

        self.frame.push_wasm_stack(tp);
    }

    /// Generates mul instruction
    pub fn gen_wasm_mul(&mut self, tp: WASMType) {
        if tp == WASMType::PTR {
            self.frame.pop_wasm_ptr_ops();
        } else {
            self.frame.pop_wasm_stack(&tp);
            self.frame.pop_wasm_stack(&tp);
        }

        self.func.borrow_mut().gen_mul(tp);

        self.frame.push_wasm_stack(tp);
    }

    /// Generates eqz instruction
    pub fn gen_wasm_eqz(&mut self, tp: &WASMType) {
        self.frame.pop_wasm_stack(tp);
        self.func.borrow_mut().gen_eqz(tp);
    }

    /// Generates load instruction
    pub fn gen_wasm_load(&mut self, tp: WASMType) {
        self.frame.pop_wasm_stack(&WASMType::PTR);
        self.func.borrow_mut().gen_load(tp);
        self.frame.push_wasm_stack(tp);
    }

    /// Starts generating if-else block
    pub fn gen_wasm_if(&mut self) {
        self.frame.pop_wasm_stack(&WASMType::I32);
        self.func.borrow_mut().gen_if();
        self.frame.start_branch();
    }

    /// Starts generating else block
    pub fn gen_wasm_else(&mut self) {
        self.frame.end_branch();
        self.func.borrow_mut().gen_else();
        self.frame.start_branch();
    }

    /// Finishes generating if-else block
    pub fn gen_wasm_endif(&mut self) {
        self.frame.end_branch();
        self.func.borrow_mut().gen_endif();
    }

    /// Starts generating loop block
    pub fn gen_wasm_loop_start(&mut self) {
        self.func.borrow_mut().gen_loop_start();
        self.frame.start_branch();
    }

    /// Finishes generating loop block and adds branch to the beginning of loop
    pub fn gen_wasm_loop_end(&mut self) {
        self.func.borrow_mut().gen_loop_end();
        self.frame.end_branch();
    }

    /// Generates conditional exit from current loop
    pub fn gen_wasm_loop_exit(&mut self) {
        self.func.borrow_mut().gen_loop_exit();
        self.frame.check_branch();
    }


    ////////////////////////////////////////////////////////////
    // Stack manipulation

    /// Drops specified number of values located on top of stack
    pub fn drop(&mut self, count: usize) {
        self.frame.drop(count);
    }

    /// Loads value stored in WASM local on top of stack
    pub fn load_wasm_local(&mut self, loc: &WASMLocalVariableRef) {
        self.frame.load_wasm_local(loc);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Allocates Fr value on stack for result of operation
    pub fn alloc_fr_result(&mut self) {
        let val = self.frame.alloc_temp(&CircomValueType::FR);

        // loading refernce to allocated temporary stack value again to
        // make sure it will be saved after removing during call operation
        self.frame.load_ref(&val);
    }

    // /// Reloads allocted on stack Fr value for result operation
    // pub fn reload_fr_result(&mut self) {
    //     self.frame.load_ref(&self.frame.top(0));
    // }

    /// Generates Fr mul operation
    pub fn fr_mul(&mut self) {
        let mul = self.module_ref().fr().mul.clone();
        self.gen_call(&mul);
    }

    /// Generates Fr div operation
    pub fn fr_div(&mut self) {
        let div = self.module_ref().fr().div.clone();
        self.gen_call(&div);
    }

    /// Generates Fr add operation
    pub fn fr_add(&mut self) {
        let add = self.module_ref().fr().add.clone();
        self.gen_call(&add);
    }

    /// Generates Fr sub operation
    pub fn fr_sub(&mut self) {
        let sub = self.module_ref().fr().sub.clone();
        self.gen_call(&sub);
    }

    /// Generates Fr pow operation
    pub fn fr_pow(&mut self) {
        let pow = self.module_ref().fr().pow.clone();
        self.gen_call(&pow);
    }

    /// Generates Fr idiv operation
    pub fn fr_idiv(&mut self) {
        let idiv = self.module_ref().fr().idiv.clone();
        self.gen_call(&idiv);
    }

    /// Generates Fr mod operation
    pub fn fr_mod(&mut self) {
        let frmod = self.module_ref().fr().mod_.clone();
        self.gen_call(&frmod);
    }

    /// Generates Fr shl operation
    pub fn fr_shl(&mut self) {
        let shl = self.module_ref().fr().shl.clone();
        self.gen_call(&shl);
    }

    /// Generates Fr shr operation
    pub fn fr_shr(&mut self) {
        let shr = self.module_ref().fr().shr.clone();
        self.gen_call(&shr);
    }

    /// Generates Fr leq operation
    pub fn fr_leq(&mut self) {
        let leq = self.module_ref().fr().leq.clone();
        self.gen_call(&leq);
    }

    /// Generates Fr geq operation
    pub fn fr_geq(&mut self) {
        let geq = self.module_ref().fr().geq.clone();
        self.gen_call(&geq);
    }

    /// Generates Fr lt operation
    pub fn fr_lt(&mut self) {
        let lt = self.module_ref().fr().lt.clone();
        self.gen_call(&lt);
    }

    /// Generates Fr gt operation
    pub fn fr_gt(&mut self) {
        let gt = self.module_ref().fr().gt.clone();
        self.gen_call(&gt);
    }

    /// Generates Fr eq operation
    pub fn fr_eq(&mut self) {
        let eq = self.module_ref().fr().eq.clone();
        self.gen_call(&eq);
    }

    /// Generates Fr neq operation
    pub fn fr_neq(&mut self) {
        let neq = self.module_ref().fr().neq.clone();
        self.gen_call(&neq);
    }

    /// Generates Fr lor operation
    pub fn fr_lor(&mut self) {
        let lor = self.module_ref().fr().lor.clone();
        self.gen_call(&lor);
    }

    /// Generates Fr land operation
    pub fn fr_land(&mut self) {
        let land = self.module_ref().fr().land.clone();
        self.gen_call(&land);
    }

    /// Generates Fr bor operation
    pub fn fr_bor(&mut self) {
        let bor = self.module_ref().fr().bor.clone();
        self.gen_call(&bor);
    }

    /// Generates Fr band operation
    pub fn fr_band(&mut self) {
        let band = self.module_ref().fr().band.clone();
        self.gen_call(&band);
    }

    /// Generates Fr bxor operation
    pub fn fr_bxor(&mut self) {
        let bxor = self.module_ref().fr().bxor.clone();
        self.gen_call(&bxor);
    }

    /// Generates Fr neg operation
    pub fn fr_neg(&mut self) {
        let neg = self.module_ref().fr().neg.clone();
        self.gen_call(&neg);
    }

    /// Generates Fr lnot operation
    pub fn fr_lnot(&mut self) {
        let lnot = self.module_ref().fr().lnot.clone();
        self.gen_call(&lnot);
    }

    /// Generates Fr bnot operation
    pub fn fr_bnot(&mut self) {
        let bnot = self.module_ref().fr().bnot.clone();
        self.gen_call(&bnot);
    }

    /// Generates saving Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_circom_store(&mut self) {
        // calling copy function
        let copy = self.module_ref().fr().copy.clone();
        self.gen_call(&copy);
    }

    /// Generates saving multiple Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_circom_store_n(&mut self, size: usize) {
        if size == 1 {
            self.gen_circom_store();
        } else {
            // calling copyn function
            self.load_const(WASMType::I32, size as i64);
            let copyn = self.module.borrow().fr().copyn.clone();
            self.gen_call(&copyn);
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

        // if num_stack_vals != 0 {
        //     let mut stack_idx = num_stack_vals - 1;
        //     let mut wasm_stack_loaded = false;

        //     // loading pointers to FR return values to WASM stack
        //     for _ in func_ref.tp().ret_types().iter().filter(|t| t.is_fr()) {
        //         let stack_val = self.frame.top(stack_idx);
        //         let stack_val_type = self.frame.value_type(&stack_val);
        //         if !stack_val_type.is_fr() {
        //             panic!("Function call return types mismatch");
        //         }

        //         // return value address may be already in WASM stack
        //         if !stack_val.is_wasm_stack() {
        //             self.frame.gen_wasm_stack_load_no_push(stack_val);
        //             wasm_stack_loaded = true;
        //         }

        //         assert!(stack_idx > 0);
        //         stack_idx -= 1;
        //     }

        //     // loading parameters to WASM stack
        //     for _ in func_ref.tp().params() {
        //         let stack_val = self.frame.top(stack_idx);
        //         if stack_val.is_wasm_stack() {
        //             if wasm_stack_loaded {
        //                 // we can't (and don't want to) duplicate values located in WASM stack
        //                 panic!("Duplicating WASM stack values is not supported");
        //             }
        //         } else {
        //             self.frame.gen_wasm_stack_load_no_push(stack_val);
        //         }
        //         stack_idx -= 1;
        //     }
        // }

        // Generating call instruction
        self.func.borrow_mut().gen_call(&func_ref.to_wasm());

        // removing parameter values from stack
        self.frame.pop(num_stack_vals);

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
    pub fn debug_dump_state(&self, stage: &str) {
        if !is_log_enabled() {
            return;
        }

        debug_log!("");
        debug_log!("============================================================");
        debug_log!("DUMP STATE BEGIN: {}", stage);
        debug_log!("============================================================");

        debug_log!("CIRCOM STACK:");
        debug_log!("{}", self.frame.dump());
        debug_log!("");

        debug_log!("MEMORY STACK:");
        debug_log!("{}", self.mem_frame_.borrow().dump());
        debug_log!("");

        debug_log!("WASM STACK:");
        debug_log!("{}", self.func.borrow().dump_stack());
        debug_log!("");

        debug_log!("DUMP STATE END: {}", stage);
        debug_log!("============================================================");
        debug_log!("");
    }
}
