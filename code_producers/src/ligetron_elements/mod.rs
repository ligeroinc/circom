
mod arrays;
mod constants;
mod data;
mod entry;
mod func;
mod ligetron;
mod log;
mod memory_stack;
mod module;
mod stack;
mod structs;
mod temp;
mod template;
mod types;
mod value;
mod wasm;
mod wasm_local;
mod wasm_stack;

pub use template::SignalKind;
pub use template::SignalInfo;

use entry::*;
use func::*;
use log::*;
use module::*;
use template::*;
use types::*;
use wasm::*;

use crate::components::*;

pub use template::TemplateInfo;
pub use func::LocalVarInfo;

use WASMType::*;

use num_bigint_dig::BigInt;

use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;


pub use module::LigetronProducerInfo;


/// Current computation mode for generated code
#[derive(Clone, PartialEq, Eq)]
pub enum ComputationMode {
    /// fp256 mode with constraints
    ConstrainedFR,

    /// fp256 mode without constraints
    FR,

    /// i32 mode
    I32
}


pub struct LigetronProducer {
    /// Code generation parameters
    info: LigetronProducerInfo,

    /// Circom module being generated
    module: Rc<RefCell<CircomModule>>,

    /// Generator for current function being generated
    func_: Option<Rc<RefCell<CircomFunction>>>,

    /// Generator for current template being generated
    template_rc: Option<RefCell<Template>>,

    /// Type of current computation mode (FR or WASM type)
    computation_mode: ComputationMode
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new(info: &LigetronProducerInfo) -> LigetronProducer {
        if info.debug_output {
            set_log_enabled(true);
        }

        debug_log!("LigetronProducer initialization begin");

        debug_log!("IO MAP:");
        for (id, entries) in &info.io_map {
            debug_log!("IO COMPONENT {}:", id);
            for entry in entries {
                debug_log!("\tIO ENTRY:");

                debug_log!("\t\tcode: {}", entry.code);
                debug_log!("\t\toffset: {}", entry.offset);
    
                let lengths_str = entry.lengths.iter()
                    .map(|ui| {ui.to_string()})
                    .collect::<Vec<_>>()
                    .join(", ");
                debug_log!("\t\tlengths: [{}]", lengths_str);

                debug_log!("\t\tsize: {}", entry.size);
            }
        }
        debug_log!("IO MAP END");

        // creating new Circom module for generated code
        let module = Rc::new(RefCell::new(CircomModule::new(info.clone())));

        return LigetronProducer {
            info: info.clone(),
            module,
            func_: None,
            template_rc: None,
            computation_mode: ComputationMode::FR
        };
    }

    /// Sets current computation type. Returns previous type
    pub fn set_computation_mode(&mut self, mode: ComputationMode) -> ComputationMode {
        let res = self.computation_mode.clone();
        self.computation_mode = mode;
        return res;
    }

    /// Resets current computation mode to default
    pub fn reset_computation_mode(&mut self) {
        self.set_computation_mode(ComputationMode::ConstrainedFR);
    }

    /// Sets current computation type for address. Returns previous computation type
    pub fn set_addr_computation_mode(&mut self) -> ComputationMode {
        return self.set_computation_mode(ComputationMode::I32);
    }

    /// Sets current computation type to Fr. Returns previous computation type.
    pub fn set_fr_computation_mode(&mut self) -> ComputationMode {
        return self.set_computation_mode(ComputationMode::FR);
    }

    /// Sets current computation type to constrained Fr. Returns previous computation type.
    pub fn set_constrained_fr_computation_mode(&mut self) -> ComputationMode {
        return self.set_computation_mode(ComputationMode::ConstrainedFR);
    }

    /// Switches mode to Fr. Actual result mode can be Fr or constrained Fr depending
    /// on current mode.
    pub fn switch_computation_mode_to_fr(&mut self) -> ComputationMode {
        let new_mode = match self.computation_mode {
            ComputationMode::ConstrainedFR => ComputationMode::ConstrainedFR,
            _ => ComputationMode::FR
        };

        self.set_computation_mode(new_mode)
    }

    /// Sets current computation type to integer. Returns previous computation type.
    pub fn set_int_computation_mode(&mut self) -> ComputationMode {
        return self.set_computation_mode(ComputationMode::I32);
    }

    /// Returns true if current computation type is I32 or I64
    fn is_computation_type_wasm(&self) -> bool {
        match &self.computation_mode {
            ComputationMode::I32 => true,
            _ => false
        }
    }

    /// Returns true if current computation type is constrained FR
    fn is_computation_type_constrained_fr(&self) -> bool {
        match &self.computation_mode {
            ComputationMode::ConstrainedFR => true,
            _ => false
        }
    }


    ////////////////////////////////////////////////////////////
    // Constants

    /// Loads constant from global constant table with specified index to stack
    pub fn load_global_const(&mut self, const_idx: usize) {
        if self.is_computation_type_wasm() {
            let const_str = self.module_ref().constants()[const_idx].clone();
            let const_val = const_str.parse::<i32>().unwrap();
            self.func().load_i32_const(const_val);
        } else {
            self.func().load_bigint_global_const(const_idx);
        }
    }

    /// Loads u32 constant with specified value to stack
    pub fn load_u32_const(&mut self, val: usize) {
        if self.is_computation_type_wasm() {
            self.func().load_i32_const(val as i32);
        } else {
            self.func().load_bigint_i64_const(val as i64);
        }
    }


    ////////////////////////////////////////////////////////////
    // Local variables, parameters and return values

    /// Returns true if global constant can be represented as 32bit
    pub fn is_global_const_32bit(&self, idx: usize) -> bool {
        let val = self.info.field_tracking[idx].parse::<BigInt>().unwrap();
        let min_int = BigInt::from(-2147483648);
        let max_int = BigInt::from(2147483647);

        return min_int <= val && val <= max_int;
    }

    /// Returns global constant value as 32bit
    pub fn global_const_as_32bit(&self, idx: usize) -> Option<u32> {
        let val = self.info.field_tracking[idx].parse::<BigInt>().unwrap();
        let min_int = BigInt::from(-2147483648);
        let max_int = BigInt::from(2147483647);

        if min_int <= val && val <= max_int {
            return Some(val.get_limb(0) as u32);
        } else {
            return None;
        }
    }

    /// Returns true if local variable with specified index is 32bit
    pub fn is_local_var_32bit(&self, mut circom_idx: usize) -> bool {
        if self.template_rc.is_none() {
            // If we are generating function then we have to take into account parameters count.
            // This is not requried for tempaltes because Circom compiler thinks there is no
            // parameters in template run function.
            let params_count = self.func().params_size();
            if circom_idx < params_count {
                // all function parameters are bigint values
                return false;
            } else {
                // Decreasing variable number by number of parameters
                circom_idx -= params_count;
            }
        }

        // searching for local variable containing specified index

        let circom_var_offset = circom_idx;
        let mut curr_idx: usize = 0;
        let mut real_var_idx: usize = 0;

        loop {
            let var = self.func().circom_locals()[real_var_idx].clone();
            let var_size = match self.func().local_var_type(&var) {
                CircomValueType::FR => 1,
                CircomValueType::Array(tp, size) => {
                    match tp.as_ref() {
                        CircomValueType::FR => size,
                        _ => {
                            panic!("Circom variable can't be an array of WASM types")
                        }
                    }
                }
                CircomValueType::WASM(..) => 1,
                CircomValueType::Struct(..) => {
                    panic!("Circom variable can't be a struct");
                }
            };

            if curr_idx + var_size > circom_var_offset {
                return self.func().local_var_type(&var).is_wasm();
            }

            real_var_idx += 1;
            curr_idx += var_size;
        }
    }

    /// Loads reference to parameter on stack using offset located on top of stack
    fn load_par_ref(&mut self, size: usize) {
        let circom_par_offset = self.func().top_index_offset(0) as usize;
        let mut curr_idx: usize = 0;
        let mut real_par_idx: usize = 0;

        loop {
            let par = self.func().param(real_par_idx).clone();
            let par_size = match self.func().param_type(&par) {
                CircomValueType::FR => 1,
                CircomValueType::Array(tp, size) => {
                    match tp.as_ref() {
                        CircomValueType::FR => size,
                        _ => {
                            panic!("Circom parameter can't be an array of WASM types")
                        }
                    }
                }
                CircomValueType::WASM(..) => 1,
                CircomValueType::Struct(..) => {
                    panic!("Circom parameter can't be a struct");
                }
            };

            if curr_idx + par_size > circom_par_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                self.func().load_i32_const((curr_idx as i32) * -1);
                self.func().index_add();

                if size == 1 {
                    if par_size == 1 {
                        self.func().drop(1);
                        self.func().load_ref(Box::new(par.clone()));
                    } else {
                        self.func().load_array_element_ref(Box::new(par.clone()));
                    }
                } else {
                    self.func().load_array_slice_ref(Box::new(par.clone()), size);
                }

                break;
            }

            real_par_idx += 1;
            curr_idx += par_size;
        }

    }

    /// Loads reference to local variable or parameter on stack
    /// using offset located on top of stack. Returns true if loaded reference is 32bit.
    pub fn load_local_var_ref(&mut self, size: usize) -> bool {
        if self.template_rc.is_none() {
            // If we are generating function then we have to take into account parameters count.
            // This is not requried for tempaltes because Circom compiler thinks there is no
            // parameters in template run function.
            let params_size = self.func().params_size();
            let offset = self.func().top_index_offset(0);
            if (offset as usize) < params_size {
                // loading parmaeter

                self.load_par_ref(size);

                // let par = self.func().param(offset as usize);
                // self.func().drop(1);
                // self.func().load_ref(Box::new(par));

                return false;
            } else {
                // Decreasing address by number of parameters. That should be done
                // in compile time in the stack manipulation logic.
                self.func().load_i32_const(-1 * (params_size as i32));
                self.func().index_add();
            }
        }

        // searching for local variable containing constant offset

        let circom_var_offset = self.func().top_index_offset(0) as usize;
        let mut curr_idx: usize = 0;
        let mut real_var_idx: usize = 0;
        let mut is_32bit = false;

        loop {
            let var = self.func().circom_locals()[real_var_idx].clone();
            let var_size = match self.func().local_var_type(&var) {
                CircomValueType::FR => 1,
                CircomValueType::Array(tp, size) => {
                    match tp.as_ref() {
                        CircomValueType::FR => size,
                        _ => {
                            panic!("Circom variable can't be an array of WASM types")
                        }
                    }
                }
                CircomValueType::WASM(..) => 1,
                CircomValueType::Struct(..) => {
                    panic!("Circom variable can't be a struct");
                }
            };

            if curr_idx + var_size > circom_var_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                self.func().load_i32_const((curr_idx as i32) * -1);
                self.func().index_add();

                if size == 1 {
                    if var_size == 1 {
                        self.func().drop(1);
                        self.func().load_ref(Box::new(var.clone()));
                    } else {
                        self.func().load_array_element_ref(Box::new(var.clone()));
                    }
                } else {
                    self.func().load_array_slice_ref(Box::new(var.clone()), size);
                }

                let var_type = self.func().local_var_type(&var);
                is_32bit = !var_type.is_fr();
                break;
            }

            real_var_idx += 1;
            curr_idx += var_size;
        }

        return is_32bit;
    }

    /// Loads reference to return value on stack
    pub fn load_ret_val_ref(&mut self) {
        let rv = self.func().ret_val(0);
        self.func().load_ref(Box::new(rv));
    }


    ////////////////////////////////////////////////////////////
    // Functions

    /// Returns reference to current function
    pub fn func(&self) -> RefMut<CircomFunction> {
        return self.func_
            .as_ref()
            .expect("no current function")
            .borrow_mut();
    }

    /// Starts generation of new function
    pub fn new_function(&mut self,
                        name: &str,
                        locals_info: Vec<LocalVarInfo>,
                        ret_vals_size: usize) {
        assert!(self.func_.is_none());
        let func = CircomFunction::new(self.module.clone(), name.to_string(), locals_info);
        self.func_ = Some(Rc::new(RefCell::new(func)));

        // adding return values
        if ret_vals_size == 0 {
            // no return values
        } else if ret_vals_size == 1 {
            self.func().new_ret_val(CircomValueType::FR, None);
        } else {
            let fr_box = Box::new(CircomValueType::FR);
            self.func().new_ret_val(CircomValueType::Array(fr_box, ret_vals_size), None);
        }
    }

    /// Finishes function generation
    pub fn end_function(&mut self, gen_entry_exit: bool) {
        assert!(self.func_.is_some());
        self.func().generate(gen_entry_exit);
        self.func_ = None
    }


    ////////////////////////////////////////////////////////////
    // Templates

    /// Returns reference to current template being generated
    pub fn template(&self) -> RefMut<Template> {
        return self.template_rc.as_ref().expect("no current template").borrow_mut();
    }

    /// Starts generating new template
    pub fn new_template(&mut self,
                        id: usize,
                        name: &str,
                        subcomponent_count: usize,
                        locals_info: Vec<LocalVarInfo>) {
        // creating new template
        assert!(self.template_rc.is_none());
        let tgen = Template::new(self.module.clone(),
                                 self.info.templates[&id].clone(),
                                 name.to_string(),
                                 subcomponent_count,
                                 locals_info);
        self.template_rc = Some(RefCell::new(tgen));

        // saving reference to function created by template
        assert!(self.func_.is_none());
        self.func_ = Some(self.template_rc.as_ref().unwrap().borrow_mut().func_rc());
    }

    /// Finishes generating template
    pub fn end_template(&mut self) {
        // generating final template code in function
        self.template().end();

        // finishing function generation and adding it to module
        self.end_function(true);

        // removing current template generator
        self.template_rc = None;
    }

    /// Loads reference to signal on stack
    pub fn load_signal_ref(&mut self, size: usize) {
        let circom_signal_offset = self.func().top_index_offset(0) as usize;

        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig_size = self.template().signal_size(real_sig_idx);

            if curr_idx + sig_size > circom_signal_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                self.func().load_i32_const((curr_idx as i32) * -1);
                self.func().index_add();
                self.template().load_signal_ref(real_sig_idx, size);

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }

    /// Creates new subcomponent
    pub fn create_subcmp(&mut self, subcmp_id: usize, template_id: usize, size: usize) {
        let templ = self.info.templates.get(&template_id)
            .expect("can't find template with required ID for subcomponent");
        self.template().create_subcmp_n(subcmp_id, templ.clone(), size);
    }

    /// Loads address of subcomponent
    pub fn load_subcmp_address(&mut self) {
        self.template().load_subcmp_address();
    }

    /// Loads reference to subcomponent indexed signal
    pub fn load_subcmp_signal_ref(&mut self, size: usize) {
        self.template().load_subcmp_signal_ref(size);
    }

    /// Loads reference to subcomponent mapped signal
    pub fn load_subcmp_mapped_signal_ref(&mut self, signal_code: usize, indexes_count: usize) {
        self.template().load_subcmp_mapped_signal_ref(signal_code, indexes_count);
    }

    /// Generates code for running subcomponent after store to signal
    pub fn gen_subcmp_run(&mut self, sig_size: usize, is_mapped: bool) {
        self.template().gen_subcmp_run(Some(sig_size), is_mapped);
    }


    ////////////////////////////////////////////////////////////

    /// Generates assert operation
    pub fn assert(&self) {
        panic!("Not implemented for fp256");
        // let assert_one = self.module_ref().ligetron().assert_one.clone();
        // self.func().gen_call(&self.module_ref().fr().is_true);
        // self.func().gen_call(&CircomFunctionRef::from_wasm(&assert_one));
    }

    /// Generates assert equal operation
    pub fn assert_equal(&self) {
        self.func().assert_equal();
    }

    /// Returns reference to Circom module being generated
    pub fn module_ref(&self) -> Ref<CircomModule> {
        return self.module.as_ref().borrow();
    }

    /// Returns mutable reference to Circom module being generated
    pub fn module_mut(&mut self) -> RefMut<CircomModule> {
        return self.module.as_ref().borrow_mut();
    }

    /// Generates final code for module
    pub fn generate(&mut self) -> Vec<String> {
        return self.module_mut().generate();
    }


    ////////////////////////////////////////////////////////////
    // Utility instructions

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func().gen_wasm_comment(str);
    }


    ////////////////////////////////////////////////////////////
    // Utility functions

    /// Generates entry point function for program.
    pub fn generate_entry(&mut self, func_name: String) {
        generate_entry(self.module.clone(), func_name);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Generates saving Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn store(&mut self, with_witness: bool) {
        if self.is_computation_type_wasm() {
            if with_witness {
                panic!("can't generate wasm store with witness");
            }
            self.func().gen_int_store();
        } else {
            self.func().gen_store_fr_with_witness();
        }
    }

    /// Generates saving multiple Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn store_n(&mut self, size: usize, with_witness: bool) {
        if size == 1 {
            self.store(with_witness);
        } else {
            if self.is_computation_type_wasm() {
                panic!("store n is not supported for wasm types");
            } else {
                self.func().gen_fr_store_n(size, with_witness);
            }
        }
    }

    /// Drops value from stack
    pub fn drop(&mut self) {
        self.func().drop(1);
    }

    /// Allocates value of stack for result of computation depending on current
    /// computation type
    pub fn alloc_result(&mut self) {
        if self.is_computation_type_wasm() {
            // we don't need to allocate result for WASM computations
        } else {
            self.func().alloc_fr_result();
        }
    }

    /// Generates mul operation
    pub fn gen_mul(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_mul();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_mul();
        } else {
            self.func().gen_fr_mul_raw();
        }
    }

    /// Generates div operation
    pub fn gen_div(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_div();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_div();
        } else {
            self.func().gen_fr_div_raw();
        }
    }

    /// Generates add operation
    pub fn gen_add(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_add();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_add();
        } else {
            self.func().gen_fr_add_raw();
        }
    }

    /// Generates sub operation
    pub fn gen_sub(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_sub();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_sub();
        } else {
            self.func().gen_fr_sub_raw();
        }
    }

    /// Generates pow operation
    pub fn gen_pow(&mut self) {
        if self.is_computation_type_wasm() {
            panic!("pow operation is not supported for wasm types");
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_pow();
        } else {
            self.func().gen_fr_pow_raw();
        }
    }

    /// Generates idiv operation
    pub fn gen_idiv(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_div();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_idiv();
        } else {
            self.func().gen_fr_idiv_raw();
        }
    }

    /// Generates mod operation
    pub fn gen_mod(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_mod();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_mod();
        } else {
            self.func().gen_fr_mod_raw();
        }
    }

    /// Generates shl operation
    pub fn gen_shl(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_shl();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_shl();
        } else {
            self.func().gen_fr_shl_raw();
        }
    }

    /// Generates shr operation
    pub fn gen_shr(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_shr();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_shr();
        } else {
            self.func().gen_fr_shr_raw();
        }
    }

    /// Generates Fr leq operation
    pub fn gen_leq(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_leq();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_leq();
        } else {
            self.func().gen_fr_leq_raw();
        }
    }

    /// Generates Fr geq operation
    pub fn gen_geq(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_geq();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_geq();
        } else {
            self.func().gen_fr_geq_raw();
        }
    }

    /// Generates Fr lt operation
    pub fn gen_lt(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_lt();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_lt();
        } else {
            self.func().gen_fr_lt_raw();
        }
    }

    /// Generates Fr gt operation
    pub fn gen_gt(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_gt();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_gt();
        } else {
            self.func().gen_fr_gt_raw();
        }
    }

    /// Generates Fr eq operation
    pub fn gen_eq(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_eq();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_eq();
        } else {
            self.func().gen_fr_eq_raw();
        }
    }

    /// Generates Fr neq operation
    pub fn gen_neq(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_neq();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_neq();
        } else {
            self.func().gen_fr_neq_raw();
        }
    }

    /// Generates Fr lor operation
    pub fn gen_lor(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_lor();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_lor();
        } else {
            self.func().gen_fr_lor_raw();
        }
    }

    /// Generates Fr land operation
    pub fn gen_land(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_land();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_land();
        } else {
            self.func().gen_fr_land_raw();
        }
    }

    /// Generates Fr bor operation
    pub fn gen_bor(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_bor();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_bor();
        } else {
            self.func().gen_fr_bor_raw();
        }
    }

    /// Generates Fr band operation
    pub fn gen_band(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_band();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_band();
        } else {
            self.func().gen_fr_band_raw();
        }
    }

    /// Generates Fr bxor operation
    pub fn gen_bxor(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_bxor();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_bxor();
        } else {
            self.func().gen_fr_bxor_raw();
        }
    }

    /// Generates Fr neg operation
    pub fn gen_neg(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_neg();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_neg();
        } else {
            self.func().gen_fr_neg_raw();
        }
    }

    /// Generates Fr lnot operation
    pub fn gen_lnot(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_lnot();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_lnot();
        } else {
            self.func().gen_fr_lnot_raw();
        }
    }

    /// Generates Fr bnot operation
    pub fn gen_bnot(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_int_bnot();
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_bnot();
        } else {
            self.func().gen_fr_bnot_raw();
        }
    }

    /// Generates bit extract operation
    pub fn gen_bit_extract(&mut self) {
        if self.is_computation_type_wasm() {
            panic!("bit extract must be done in Fr mode");
        } else if self.is_computation_type_constrained_fr() {
            self.func().gen_fr_bit_exctact();
        } else {
            self.func().gen_fr_bit_exctact_raw();
        }
    }

    /// Generates call operation
    pub fn call(&mut self, symbol: &String, args_count: usize) {
        // constructing function type
        let arg_types = std::iter::repeat([CircomValueType::FR])
            .flatten().take(args_count).collect::<Vec<_>>();
        let ftype = CircomFunctionType::new(arg_types, vec![CircomValueType::FR]);

        // constructing function reference
        let func = CircomFunctionRef::new(symbol.clone(), ftype);

        // generating call
        self.func().gen_call(&func, !self.is_computation_type_constrained_fr());
    }


    ////////////////////////////////////////////////////////////
    // Addresses

    /// Generates conversion of stack top value to address
    pub fn to_address(&mut self) {
        if self.is_computation_type_wasm() {
            // converting top stack value to index
            self.func().convert_index();
        } else {
            panic!("conversion to address should be done in 32bit mode");
        }
    }

    /// Generates address add operation
    pub fn addr_add(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().index_add();
        } else {
            panic!("index add should be done in 32bit mode");
        }
    }

    /// Generates address mul opearation
    pub fn addr_mul(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().index_mul();
        } else {
            panic!("index mul should be done in 32bit mode");
        }
    }


    ////////////////////////////////////////////////////////////
    // Branch operations

    /// Starts generating if-else block using current stack value as condition
    pub fn gen_if(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_if();
        } else {
            panic!("if condition should be generated in int mode");
        }
    }

    /// Starts generating else block
    pub fn gen_else(&mut self) {
        self.func().gen_wasm_else();
    }

    /// Finishes generating if-else block
    pub fn gen_endif(&mut self) {
        self.func().gen_wasm_endif();
    }

    /// Starts generating loop block
    pub fn gen_loop_start(&mut self) {
        self.func().gen_wasm_loop_start();
    }

    /// Finishes generating loop block and adds branch to the beginning of loop
    pub fn gen_loop_end(&mut self) {
        self.func().gen_wasm_loop_end();
    }

    /// Generates conditional exit from current loop using current FR value on stack
    /// as loop condition
    pub fn gen_loop_exit(&mut self) {
        if self.is_computation_type_wasm() {
            self.func().gen_eqz();
            self.func().get_frame_mut().pop(1);
            self.func().gen_wasm_loop_exit();
        } else {
            panic!("loop exit should always be generated in 32bit mode");
        }
    }


    ////////////////////////////////////////////////////////////
    // Logging

    /// Generates logging of string message
    pub fn log_str(&mut self, str_idx: usize) {
        // looking of string offset in string table
        let str_offset = self.module_ref().string_address(str_idx);

        // generating call to Ligetron print_str function
        let print_str = self.module_ref().ligetron().print_str.clone();
        self.func().gen_wasm_comment("logging string");
        self.func().gen_wasm_const(PTR, str_offset as i64);
        self.func().gen_wasm_const(I32, self.info.string_table[str_idx].len() as i64);
        self.func().gen_wasm_call(&print_str);
    }

    /// Generates logging of value located on stack
    pub fn log_val(&mut self) {
        self.func().gen_wasm_comment("logging value");

        // generating call to Ligetron fp256_print function
        let fp256_print = self.module_ref().ligetron().fp256_print.clone();
        self.func().gen_call(&fp256_print, false);
    }


    ////////////////////////////////////////////////////////////
    // Debugging

    /// Dumps current generator state for debugging
    pub fn debug_dump_state(&self, stage: &str) {
        self.func().debug_dump_state(stage);
        debug_log!("32BIT: {}", self.is_computation_type_wasm());
    }

    /// Dumps current stack contents to string
    pub fn dump_stack(&self) -> String {
        return self.func().get_frame().dump();
    }
}

impl Default for LigetronProducerInfo {
    fn default() -> Self {
        return LigetronProducerInfo {
            const_size: 32,
            templates: HashMap::new(),
            main_comp_id: 0,
            string_table: vec![],
            field_tracking: vec![],
            debug_output: false,
            io_map: TemplateInstanceIOMap::new()
        };
    }
}
