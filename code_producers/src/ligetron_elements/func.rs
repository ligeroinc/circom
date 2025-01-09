
use super::arrays::*;
use super::log::*;
use super::memory_stack::*;
use super::stack::*;
use super::types::*;
use super::wasm::*;
use super::CircomModule;

use num_bigint_dig::BigInt;

use std::rc::Rc;
use std::cell::{RefCell, RefMut};


pub struct LocalVarInfo {
    pub size: usize,
    pub is_32bit: bool,
}


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

    /// Array of Circom local variables referenced in Circom IR
    circom_locals: Vec<CircomLocalVariableRef>
}

impl CircomFunction {
    /// Constructs new function generator
    pub fn new(module: Rc<RefCell<CircomModule>>,
               name: String,
               locals_info: Vec<LocalVarInfo>) -> CircomFunction {

        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("CIRCOM FUNCTION BEGIN: {}", name);
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        for (idx, var) in locals_info.iter().enumerate() {
            debug_log!("VAR {}: {}, {}", idx, var.is_32bit, var.size);
        }

        // creating underlying WASM function
        let func = WASMFunction::new(module.borrow_mut().wasm_module_rc().clone(), name);

        // creating new memory stack frame for this funcion
        let mem_frame = Rc::new(RefCell::new(MemoryStackFrame::new(module.borrow().wasm_module_rc().clone(),
                                                                   module.borrow().stack_ptr().clone(),
                                                                   func.inst_gen_rc().clone(),
                                                                   func.frame_rc().clone())));

        let func_rc = Rc::new(RefCell::new(func));

        let frame = CircomStackFrame::new(module.clone(), func_rc.clone(), mem_frame.clone());

        let mut func = CircomFunction {
            module: module,
            func: func_rc.clone(),
            mem_frame_: mem_frame.clone(),
            frame: frame,
            circom_locals: vec![]
        };

        func.init(locals_info);

        return func;
    }

    /// Initializes function
    fn init(&mut self, locals_info: Vec<LocalVarInfo>) {
        // Allocating Circom local variables
        for (idx, loc_info) in locals_info.iter().enumerate() {
            let tp = if loc_info.is_32bit {
                if loc_info.size == 1 {
                    CircomValueType::WASM(WASMType::I32)
                } else {
                    panic!("32bit arrays are not supported");
                }
            } else {
                if loc_info.size == 1 {
                    CircomValueType::FR
                } else {
                    CircomValueType::FRArray(loc_info.size)
                }
            };

            let _ = self.new_circom_local_var(idx, tp);
        }
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
            self.frame.gen_func_entry();
            self.frame.gen_func_exit();
        }

        self.module_ref().append_instruction(&mut vec![format!("")]);
        self.module_ref().append_instruction(&mut self.func.borrow_mut().generate());
    }


    ////////////////////////////////////////////////////////////
    /// Constants

    /// Loads WASM constant value on WASM stack
    pub fn load_wasm_const(&mut self, tp: WASMType, val: i64) {
        self.frame.load_wasm_const(tp, val);
    }

    /// Loads i32 const value on stack
    pub fn load_i32_const(&mut self, val: i32) {
        self.frame.load_i32_const(val);
    }

    /// Loads bigint const from global constant table
    pub fn load_bigint_global_const(&mut self, const_idx: usize) {
        let addr = self.module_ref().constant_address(const_idx);
        let tmp_val = self.alloc_temp(CircomValueType::FR);
        self.load_temp_value_ptr_to_wasm_stack(&tmp_val);
        self.gen_wasm_const(WASMType::PTR, addr as i64);
        let csize = self.module_ref().const_size();
        self.gen_wasm_const(WASMType::I32, csize as i64);
        let fp256_from_hex = self.module_ref().ligetron().fp256_from_hex.to_wasm();
        self.gen_wasm_call(&fp256_from_hex);
    }

    /// Loads bigint const from i64 value
    pub fn load_bigint_i64_const(&mut self, val: i64) {
        let tmp_val = self.alloc_temp(CircomValueType::FR);
        self.load_ref(tmp_val);
        self.load_wasm_const(WASMType::I64, val);
        let fp256_set_ui = self.module_ref().ligetron().fp256_set_ui.clone();
        self.gen_call(&fp256_set_ui);
    }


    ////////////////////////////////////////////////////////////
    /// Stack values operations

    /// Loads value pointer onto WASM stack
    pub fn value_load_ptr_to_wasm_stack(&mut self, val: &CircomStackValueRef) {
        self.frame.value_load_ptr_to_wasm_stack(val);
    }


    ////////////////////////////////////////////////////////////
    /// Local variables

    /// Creates new local variable
    pub fn new_local_var(&mut self, name: String, tp: CircomValueType) -> CircomLocalVariableRef {
        return self.frame.new_local_var(name, tp);
    }

    /// Returns local variable type
    pub fn local_var_type(&self, var: &CircomLocalVariableRef) -> CircomValueType {
        return self.frame.local_var_type(var).clone();
    }

    /// Creates new Circom local variable with specified variable index
    pub fn new_circom_local_var(&mut self,
                                idx: usize,
                                tp: CircomValueType) -> CircomLocalVariableRef {
        let var = self.new_local_var(format!("local_var_{}", idx), tp);
        self.circom_locals.push(var.clone());
        return var;
    }

    /// Returns reference to array of circom local variables referenced from IR by indexes
    pub fn circom_locals(&self) -> &Vec<CircomLocalVariableRef> {
        return &self.circom_locals;
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

    /// Loads pointer to temporary value onto WASM stack
    pub fn load_temp_value_ptr_to_wasm_stack(&mut self, val: &TemporaryStackValueRef) {
        self.frame.load_temp_value_ptr_to_wasm_stack(val);
    }


    ////////////////////////////////////////////////////////////
    /// References

    /// Loads reference to value on stack
    pub fn load_ref<T: CircomValueRef + 'static>(&mut self, val: T) {
        self.frame.load_ref(val);
    }

    /// Loads reference to subarray of array to stack
    pub fn load_array_slice_ref<T: CircomValueRef + 'static>(&mut self, arr: T, size: usize) {
        self.frame.load_array_slice_ref(arr, size);
    }

    /// Loads reference to element of array to stack using top stack address value as offset
    pub fn load_array_element_ref<T: CircomValueRef + 'static>(&mut self, arr: T) {
        self.frame.load_array_element_ref(arr);
    }

    /// Generates loading of pointer to array element with const index onto WASM stack
    pub fn gen_load_array_element_ptr_to_wasm_stack_const(&mut self,
                                                          val: &TemporaryStackValueRef,
                                                          index: usize) {
        gen_load_array_element_ptr_to_wasm_stack_const(&mut self.frame, val, index);
    }


    ////////////////////////////////////////////////////////////
    /// Addresses and indexes

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

    /// Adds new WASM local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, tp: WASMType) -> WASMLocalVariableRef {
        return self.func.borrow_mut().new_wasm_local(tp);
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
        self.func.borrow_mut().gen_call(func);
    }

    /// Generates getting value of a local
    pub fn gen_wasm_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        self.func.borrow_mut().gen_local_get(var_ref);
    }

    /// Generates setting value of a local
    pub fn gen_wasm_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        self.func.borrow_mut().gen_local_set(var_ref);
    }

    /// Generates getting value of a global
    pub fn gen_wasm_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.func.borrow_mut().gen_global_get(var_ref);
    }

    /// Generates setting value of a global
    pub fn gen_wasm_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.func.borrow_mut().gen_global_set(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_wasm_drop(&mut self) {
        self.func.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_wasm_const(&mut self, tp: WASMType, value: i64) {
        self.func.borrow_mut().gen_const(tp.clone(), value);
    }

    /// Generates add instruction
    pub fn gen_wasm_add(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_add(tp);
    }

    /// Generates sub instruction
    pub fn gen_wasm_sub(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_sub(tp);
    }

    /// Generates mul instruction
    pub fn gen_wasm_mul(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_mul(tp);
    }

    /// Generates eqz instruction
    pub fn gen_wasm_eqz(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_eqz(&tp);
    }

    /// Generates eq instruction
    pub fn gen_wasm_eq(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_eq(&tp);
    }

    /// Generates lt_u instruction
    pub fn gen_wasm_lt_u(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_lt_u(&tp);
    }

    /// Generates load instruction
    pub fn gen_wasm_load(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_load(tp);
    }

    /// Generates store instruction
    pub fn gen_wasm_store(&mut self, tp: WASMType) {
        self.func.borrow_mut().gen_store(&tp);
    }

    /// Starts generating if-else block
    pub fn gen_wasm_if(&mut self) {
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


    ////////////////////////////////////////////////////////////
    // Integer operations

    /// Generates int binary operation
    fn gen_int_bin_op<Op: FnOnce(RefMut<WASMFunction>) -> ()>(&mut self, op: Op) {
        let i32t = CircomValueType::WASM(WASMType::I32);
        let types = vec![i32t.clone(), i32t.clone()];
        self.frame.gen_op(types, false, op);

        // adding result WASM value to logical stack
        self.frame.push_wasm_stack(WASMType::I32);
    }

    /// Generates int comparison opearation
    fn gen_int_cmp_op<Op: FnOnce(RefMut<WASMFunction>) -> ()>(&mut self, op: Op) {
        let i32t = CircomValueType::WASM(WASMType::I32);
        let types = vec![i32t.clone(), i32t.clone()];
        self.frame.gen_op(types, false, op);
    }

    /// Generates int mul operation
    pub fn gen_int_mul(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_mul(WASMType::I32);
        });
    }

    /// Generates int div operation
    pub fn gen_int_div(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_div_u(WASMType::I32);
        });
    }

    /// Generates int add operation
    pub fn gen_int_add(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_add(WASMType::I32);
        });
    }

    /// Generates int sub operation
    pub fn gen_int_sub(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_sub(WASMType::I32);
        });
    }

    /// Generates int pow operation
    pub fn gen_int_pow(&mut self) {
        panic!("pow operation is not implemented yet for int");
    }

    /// Generates int mod operation
    pub fn gen_int_mod(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_div_u(WASMType::I32);
        });
    }

    /// Generates int shl operation
    pub fn gen_int_shl(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_shl(WASMType::I32);
        });
    }

    /// Generates int shr operation
    pub fn gen_int_shr(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_shr_u(WASMType::I32);
        });
    }

    /// Generates int leq operation
    pub fn gen_int_leq(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_le_u(&WASMType::I32);
        });
    }

    /// Generates int geq operation
    pub fn gen_int_geq(&mut self) {
        self.gen_int_bin_op(|mut func| {
            func.gen_ge_u(&WASMType::I32);
        });
    }

    /// Generates int lt operation
    pub fn gen_int_lt(&mut self) {
        self.gen_int_cmp_op(|mut func| {
            func.gen_lt_u(&WASMType::I32);
        });
    }

    /// Generates int gt operation
    pub fn gen_int_gt(&mut self) {
        self.gen_int_cmp_op(|mut func| {
            func.gen_gt_u(&WASMType::I32);
        });
    }

    /// Generates int eq operation
    pub fn gen_int_eq(&mut self) {
        self.gen_int_cmp_op(|mut func| {
            func.gen_eq(&WASMType::I32);
        });
    }

    /// Generates int neq operation
    pub fn gen_int_neq(&mut self) {
        self.gen_int_cmp_op(|mut func| {
            func.gen_ne(&WASMType::I32);
        });
    }

    /// Generates int lor operation
    pub fn gen_int_lor(&mut self) {
        panic!("lor operation is not supported for int types");
    }

    /// Generates int land operation
    pub fn gen_int_land(&mut self) {
        panic!("lor operation is not supported for int types");
    }

    /// Generates int bor operation
    pub fn gen_int_bor(&mut self) {
        panic!("bor operation is not supported for int types");
    }

    /// Generates int band operation
    pub fn gen_int_band(&mut self) {
        panic!("band operation is not supported for int types");
    }

    /// Generates int bxor operation
    pub fn gen_int_bxor(&mut self) {
        panic!("bxor operation is not supported for int types");
    }

    /// Generates int neg operation
    pub fn gen_int_neg(&mut self) {
        panic!("neg operation is not supported for int types");
    }

    /// Generates int lnot operation
    pub fn gen_int_lnot(&mut self) {
        panic!("lnot operation is not supported for int types");
    }

    /// Generates int bnot operation
    pub fn gen_int_bnot(&mut self) {
        panic!("bnot operation is not supported for int types");
    }


    ////////////////////////////////////////////////////////////
    // Fr operations

    /// Allocates Fr value on stack for result of operation
    pub fn alloc_fr_result(&mut self) {
        let val = self.frame.alloc_temp(&CircomValueType::FR);

        // loading refernce to allocated temporary stack value again to
        // make sure it will be saved after removing during call operation
        self.frame.load_ref(val);
    }

    // /// Reloads allocted on stack Fr value for result operation
    // pub fn reload_fr_result(&mut self) {
    //     self.frame.load_ref(&self.frame.top(0));
    // }

    /// Generates Fr binary operation
    fn gen_fr_bin_op(&mut self, op_func: WASMFunctionRef) {
        let types = vec![CircomValueType::FR, CircomValueType::FR, CircomValueType::FR];
        self.frame.gen_op(types, false, |mut func| {
            func.gen_call(&op_func);
        });
    }

    /// Generates Fr mul operation
    pub fn gen_fr_mul(&mut self) {
        let func = self.module_ref().ligetron().fp256_mulmod.to_wasm();
        self.gen_fr_bin_op(func);
    }

    /// Generates Fr div operation
    pub fn gen_fr_div(&mut self) {
        let func = self.module_ref().ligetron().fp256_divmod.to_wasm();
        self.gen_fr_bin_op(func);
    }

    /// Generates Fr add operation
    pub fn gen_fr_add(&mut self) {
        let func = self.module_ref().ligetron().fp256_addmod.to_wasm();
        self.gen_fr_bin_op(func);
    }

    /// Generates Fr sub operation
    pub fn gen_fr_sub(&mut self) {
        let func = self.module_ref().ligetron().fp256_submod.to_wasm();
        self.gen_fr_bin_op(func);
    }

    /// Generates Fr pow operation
    pub fn gen_fr_pow(&mut self) {
        panic!("pow operation is not implemented for fp256 yet");
        // let pow = self.module_ref().fr().pow.clone();
        // self.gen_call(&pow);
    }

    /// Generates Fr idiv operation
    pub fn gen_fr_idiv(&mut self) {
        panic!("idiv operation is not implemented for fp256 yet");
        // let idiv = self.module_ref().fr().idiv.clone();
        // self.gen_call(&idiv);
    }

    /// Generates Fr mod operation
    pub fn gen_fr_mod(&mut self) {
        panic!("mod operation is not implemented for fp256 yet");
        // let frmod = self.module_ref().fr().mod_.clone();
        // self.gen_call(&frmod);
    }

    /// Generates Fr shl operation
    pub fn gen_fr_shl(&mut self) {
        panic!("shl operation is not implemented for fp256 yet");
        // let shl = self.module_ref().fr().shl.clone();
        // self.gen_call(&shl);
    }

    /// Generates Fr shr operation
    pub fn gen_fr_shr(&mut self) {
        panic!("shr operation is not implemented for fp256 yet");
        // let shr = self.module_ref().fr().shr.clone();
        // self.gen_call(&shr);
    }

    /// Generates Fr leq operation
    pub fn gen_fr_leq(&mut self) {
        panic!("leq operation is not implemented for fp256 yet");
        // let leq = self.module_ref().fr().leq.clone();
        // self.gen_call(&leq);
    }

    /// Generates Fr geq operation
    pub fn gen_fr_geq(&mut self) {
        panic!("geq operation is not implemented for fp256 yet");
        // let geq = self.module_ref().fr().geq.clone();
        // self.gen_call(&geq);
    }

    /// Generates Fr lt operation
    pub fn gen_fr_lt(&mut self) {
        panic!("lt operation is not implemented for fp256 yet");
        // let lt = self.module_ref().fr().lt.clone();
        // self.gen_call(&lt);
    }

    /// Generates Fr gt operation
    pub fn gen_fr_gt(&mut self) {
        panic!("Not implemented for fp256");
        // let gt = self.module_ref().fr().gt.clone();
        // self.gen_call(&gt);
    }

    /// Generates Fr eq operation
    pub fn gen_fr_eq(&mut self) {
        let fp256_assert_equal = self.module_ref().ligetron().fp256_assert_equal.clone();
        self.gen_call(&fp256_assert_equal);
    }

    /// Generates Fr neq operation
    pub fn gen_fr_neq(&mut self) {
        panic!("Not implemented for fp256");
        // let neq = self.module_ref().fr().neq.clone();
        // self.gen_call(&neq);
    }

    /// Generates Fr lor operation
    pub fn gen_fr_lor(&mut self) {
        panic!("Not implemented for fp256");
        // let lor = self.module_ref().fr().lor.clone();
        // self.gen_call(&lor);
    }

    /// Generates Fr land operation
    pub fn gen_fr_land(&mut self) {
        panic!("Not implemented for fp256");
        // let land = self.module_ref().fr().land.clone();
        // self.gen_call(&land);
    }

    /// Generates Fr bor operation
    pub fn gen_fr_bor(&mut self) {
        panic!("Not implemented for fp256");
        // let bor = self.module_ref().fr().bor.clone();
        // self.gen_call(&bor);
    }

    /// Generates Fr band operation
    pub fn gen_fr_band(&mut self) {
        panic!("Not implemented for fp256");
        // let band = self.module_ref().fr().band.clone();
        // self.gen_call(&band);
    }

    /// Generates Fr bxor operation
    pub fn gen_fr_bxor(&mut self) {
        panic!("Not implemented for fp256");
        // let bxor = self.module_ref().fr().bxor.clone();
        // self.gen_call(&bxor);
    }

    /// Generates Fr neg operation
    pub fn gen_fr_neg(&mut self) {
        panic!("Not implemented for fp256");
        // let neg = self.module_ref().fr().neg.clone();
        // self.gen_call(&neg);
    }

    /// Generates Fr lnot operation
    pub fn gen_fr_lnot(&mut self) {
        panic!("Not implemented for fp256");
        // let lnot = self.module_ref().fr().lnot.clone();
        // self.gen_call(&lnot);
    }

    /// Generates Fr bnot operation
    pub fn gen_fr_bnot(&mut self) {
        panic!("Not implemented for fp256");
        // let bnot = self.module_ref().fr().bnot.clone();
        // self.gen_call(&bnot);
    }

    /// Generates store of int value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_int_store(&mut self) {
        // generating store operation
        // NOTE: passing true to gen_op function to indicate that first argument should
        // be loaded as pointer
        let types = vec![CircomValueType::WASM(WASMType::I32),
                         CircomValueType::WASM(WASMType::I32)];
        self.frame.gen_op(types, true, |mut func| {
            func.gen_store(&WASMType::I32);
        });
    }

    /// Generates store of Fr value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_store_fr(&mut self) {
        let fp256_set_fp256 = self.module_ref().ligetron().fp256_set_fp256.to_wasm();
        self.frame.gen_op(vec![CircomValueType::FR, CircomValueType::FR], false, |mut func| {
            func.gen_call(&fp256_set_fp256);
        });
    }

    /// Generates saving multiple Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_fr_store_n(&mut self, size: usize) {
        if size == 1 {
            self.gen_store_fr();
        } else {
            let src_ref = self.frame.top(0);
            let dst_ref = self.frame.top(1);

            if !self.frame.value_type(&dst_ref).is_fr() ||
               !self.frame.value_type(&src_ref).is_fr() {
                panic!("copy n supported only for bigint values");
            }

            self.frame.load_values_to_wasm_stack(2, CircomValueType::FR);

            let dst_ptr_var = self.new_wasm_local(WASMType::PTR);
            let src_ptr_var = self.new_wasm_local(WASMType::PTR);
            let src_end_ptr_var = self.new_wasm_local(WASMType::PTR);

            self.gen_wasm_comment(&format!("; store {} elements", size));

            // saving src and dst pointers
            self.gen_wasm_local_set(&src_ptr_var);
            self.gen_wasm_local_set(&dst_ptr_var);

            // calculating end source pointer
            self.gen_wasm_local_get(&src_ptr_var);
            self.gen_wasm_const(WASMType::I32, (size * CircomValueType::FR.size()) as i64);
            self.gen_wasm_add(WASMType::PTR);
            self.gen_wasm_local_set(&src_end_ptr_var);

            self.debug_dump_state("BEFORE LOOP");

            self.gen_wasm_loop_start();

            // checking for loop exit
            self.gen_wasm_local_get(&src_ptr_var);
            self.gen_wasm_local_get(&src_end_ptr_var);
            self.gen_wasm_eq(WASMType::PTR);
            self.gen_wasm_loop_exit();

            self.debug_dump_state("BEFORE COPY");

            // copy function call
            self.gen_wasm_local_get(&dst_ptr_var);
            self.gen_wasm_local_get(&src_ptr_var);
            let fp256_set_fp256 = self.module_ref().ligetron().fp256_set_fp256.to_wasm();
            self.gen_wasm_call(&fp256_set_fp256);

            self.debug_dump_state("AFTER COPY");

            // incrementing src pointer
            self.gen_wasm_local_get(&src_ptr_var);
            self.gen_wasm_const(WASMType::I32, CircomValueType::FR.size() as i64);
            self.gen_wasm_add(WASMType::PTR);
            self.gen_wasm_local_set(&src_ptr_var);

            // incrementing dst pointer
            self.gen_wasm_local_get(&dst_ptr_var);
            self.gen_wasm_const(WASMType::I32, CircomValueType::FR.size() as i64);
            self.gen_wasm_add(WASMType::PTR);
            self.gen_wasm_local_set(&dst_ptr_var);

            self.gen_wasm_loop_end();

            // popping original values from stack
            self.frame.pop(2);
        }
    }

    /// Generates assert equal operation
    pub fn assert_equal(&mut self) {
        let fp256_assert_equal = self.module_ref().ligetron().fp256_assert_equal.clone();
        self.gen_call(&fp256_assert_equal);
    }

    /// Generates call to function passing values located on top of stack as parameters
    pub fn gen_call(&mut self, func_ref: &CircomFunctionRef) {
        self.frame.gen_call(func_ref);
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
