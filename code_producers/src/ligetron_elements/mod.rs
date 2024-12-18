
mod entry;
mod func;
mod ligetron;
mod log;
mod memory_stack;
mod module;
mod stack;
mod template;
mod types;
mod wasm;

pub use template::SignalKind;
pub use template::SignalInfo;

use entry::*;
use func::*;
use log::*;
use module::*;
use template::*;
use types::*;
use wasm::*;

pub use template::TemplateInfo;

use WASMType::*;

use num_bigint_dig::BigInt;
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;


pub use module::LigetronProducerInfo;


pub struct LigetronProducer {
    /// Code generation parameters
    info: LigetronProducerInfo,

    /// Circom module being generated
    module: Rc<RefCell<CircomModule>>,

    /// Generator for current function being generated
    func_: Option<Rc<RefCell<CircomFunction>>>,

    /// Generator for current template being generated
    template_rc: Option<RefCell<Template>>
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new(info: &LigetronProducerInfo) -> LigetronProducer {
        if info.debug_output {
            set_log_enabled(true);
        }

        debug_log!("LigetronProducer initialization begin");

        // creating new Circom module for generated code
        let module = Rc::new(RefCell::new(CircomModule::new(info.clone())));

        return LigetronProducer {
            info: info.clone(),
            module,
            func_: None,
            template_rc: None,
        };
    }

    ////////////////////////////////////////////////////////////
    // Constants

    /// Loads constant with specified index to stack
    pub fn load_const(&mut self, const_idx: usize) {
        self.func().load_const(const_idx);
    }

    /// Loads u32 constant with specified value to stack
    pub fn load_const_u32(&mut self, val: usize) {
        self.func().load_const_u32(val);
    }

    /// Sets constant address mode flag. Returns previous flag value.
    pub fn set_const_addr_mode(&mut self, val: bool) -> bool {
        return self.func().set_const_addr_mode(val);
    }


    ////////////////////////////////////////////////////////////
    // Local variables, parameters and return values

    /// Loads reference to local variable or parameter on stack
    pub fn load_local_var_ref(&mut self) {
        let circom_locals = self.func().circom_locals();

        // calculating real offset of variable in array of all circom variables
        if self.template_rc.is_some() {
            // if we are generating template then variable index is real variable index
            // because Circom compiler does not count input signals as function parameters
            self.func().load_array_element_ref(&circom_locals);
        } else {
            let params_count = self.func().params_count();
            let offset = self.func().top_index_offset();

            if (offset as usize) < params_count {
                // loading parmaeter
                if !self.func().top_index_is_const() {
                    panic!("can't load parameter with not const index");
                }

                let par = self.func().param(offset as usize);
                self.func().drop(1);
                self.func().load_ref(&par);
            } else {
                // Decreasing address by number of parameters. That should be done
                // in compile time in the stack manipulation logic.
                self.func().load_const_index(-1 * (params_count as i32));
                self.func().index_add();

                // loading local variable
                self.func().load_array_element_ref(&circom_locals);
            }
        }
    }

    /// Loads reference to array of local variables or parameter on stack
    pub fn load_local_var_array_ref(&mut self, size: usize) {
        let circom_locals = self.func().circom_locals();

        if self.template_rc.is_some() {
            // if we are generating template then variable index is real variable index
            // because Circom compiler does not count input signals as function parameters
            self.func().load_array_slice_ref(&circom_locals, size);
        } else {
            let params_count = self.func().params_count();
            let offset = self.func().top_index_offset();

            if (offset as usize) < params_count {
                panic!("parameter arrays are not supported");
            } else {
                // Decreasing address by number of parameters. That should be done
                // in compile time in the stack manipulation logic.
                self.func().load_const_index(-1 * (params_count as i32));
                self.func().index_add();

                // loading local variable array slice
                self.func().load_array_slice_ref(&circom_locals, size);
            }
        }
    }

    /// Loads reference to return value on stack
    pub fn load_ret_val_ref(&mut self) {
        let rv = self.func().ret_val(0);
        self.func().load_ref(&rv);
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
                        n_local_vars: usize,
                        ret_vals_size: usize) {
        assert!(self.func_.is_none());
        let func = CircomFunction::new(self.module.clone(), name.to_string(), n_local_vars);
        self.func_ = Some(Rc::new(RefCell::new(func)));

        // adding return values
        if ret_vals_size == 0 {
            // no return values
        } else if ret_vals_size == 1 {
            self.func().new_ret_val(CircomValueType::FR, None);
        } else {
            self.func().new_ret_val(CircomValueType::FRArray(ret_vals_size), None);
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
    pub fn new_template(&mut self, name: &str,
                        signals: &Vec<SignalInfo>,
                        n_local_vars: usize) {
        // creating new template generator
        assert!(self.template_rc.is_none());
        let tgen = Template::new(self.module.clone(),
                                 name.to_string(),
                                 signals,
                                 n_local_vars);
        self.template_rc = Some(RefCell::new(tgen));

        // saving reference to function generator created by template generator
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
        let circom_signal_offset = self.func().top_index_offset() as usize;

        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig = self.template().signal(real_sig_idx);
            let sig_size = self.template().signal_size(&sig);

            if curr_idx + sig_size > circom_signal_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                self.func().load_const_index((curr_idx as i32) * -1);
                self.func().index_add();
                self.template().load_signal_ref(&sig, size);

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }

    /// Creates new subcomponent
    pub fn create_subcmp(&mut self, subcmp_id: usize, template_id: usize) {
        let templ = self.info.templates.get(&template_id)
            .expect("can't find template with required ID for subcomponent");
        self.template().create_subcmp(subcmp_id, template_id, &templ);
    }

    /// Loads reference to subcomponent signal
    pub fn load_subcmp_signal_ref(&mut self, subcmp_idx: usize, size: usize) {
        self.template().load_subcmp_signal_ref(subcmp_idx, size);
    }

    /// Generates code for running subcomponent after store to signal
    pub fn gen_subcmp_run(&mut self, subcmp_idx: usize, sig_size: usize) {
        let tid = self.template().subcmp_template_id(subcmp_idx);
        let run_func = self.module_ref().templ_run_function(tid);
        self.template().gen_subcmp_run(subcmp_idx, sig_size, run_func);
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
    pub fn store(&mut self) {
        self.func().gen_circom_store();
    }

    /// Generates saving multiple Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn store_n(&mut self, size: usize) {
        self.func().gen_circom_store_n(size);
    }

    /// Drops value from stack
    pub fn drop(&mut self) {
        self.func().drop(1);
    }

    /// Allocates Fr value on stack for result of operation
    pub fn alloc_fr_result(&mut self) {
        self.func().alloc_fr_result();
    }

    /// Reloads allocted on stack Fr value for result operation
    // pub fn reload_fr_result(&mut self) {
    //     self.func().reload_fr_result();
    // }

    /// Generates Fr mul operation
    pub fn fr_mul(&mut self) {
        self.func().fr_mul();
    }

    /// Generates Fr div operation
    pub fn fr_div(&mut self) {
        self.func().fr_div();
    }

    /// Generates Fr add operation
    pub fn fr_add(&mut self) {
        self.func().fr_add();
    }

    /// Generates Fr sub operation
    pub fn fr_sub(&mut self) {
        self.func().fr_sub();
    }

    /// Generates Fr pow operation
    pub fn fr_pow(&mut self) {
        self.func().fr_pow();
    }

    /// Generates Fr idiv operation
    pub fn fr_idiv(&mut self) {
        self.func().fr_idiv();
    }

    /// Generates Fr mod operation
    pub fn fr_mod(&mut self) {
        self.func().fr_mod();
    }

    /// Generates Fr shl operation
    pub fn fr_shl(&mut self) {
        self.func().fr_shl();
    }

    /// Generates Fr shr operation
    pub fn fr_shr(&mut self) {
        self.func().fr_shr();
    }

    /// Generates Fr leq operation
    pub fn fr_leq(&mut self) {
        self.func().fr_leq();
    }

    /// Generates Fr geq operation
    pub fn fr_geq(&mut self) {
        self.func().fr_geq();
    }

    /// Generates Fr lt operation
    pub fn fr_lt(&mut self) {
        self.func().fr_lt();
    }

    /// Generates Fr gt operation
    pub fn fr_gt(&mut self) {
        self.func().fr_gt();
    }

    /// Generates Fr eq operation
    pub fn fr_eq(&mut self) {
        self.func().fr_eq();
    }

    /// Generates Fr neq operation
    pub fn fr_neq(&mut self) {
        self.func().fr_neq();
    }

    /// Generates Fr lor operation
    pub fn fr_lor(&mut self) {
        self.func().fr_lor();
    }

    /// Generates Fr land operation
    pub fn fr_land(&mut self) {
        self.func().fr_land();
    }

    /// Generates Fr bor operation
    pub fn fr_bor(&mut self) {
        self.func().fr_bor();
    }

    /// Generates Fr band operation
    pub fn fr_band(&mut self) {
        self.func().fr_band();
    }

    /// Generates Fr bxor operation
    pub fn fr_bxor(&mut self) {
        self.func().fr_bxor();
    }

    /// Generates Fr neg operation
    pub fn fr_neg(&mut self) {
        self.func().fr_neg();
    }

    /// Generates Fr lnot operation
    pub fn fr_lnot(&mut self) {
        self.func().fr_lnot();
    }

    /// Generates Fr bnot operation
    pub fn fr_bnot(&mut self) {
        self.func().fr_bnot();
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
        self.func().gen_call(&func);
    }


    ////////////////////////////////////////////////////////////
    // Addresses

    /// Generates conversion of stack top value to address
    pub fn to_address(&mut self) {
        panic!("Not implemented for fp256");
        // // converting value to address with Fr_toInt function
        // self.func().gen_call(&self.module_ref().fr().to_int);

        // // converting WASM I32 value to address
        // self.func().convert_index();
    }

    /// Generates address add operation
    pub fn addr_add(&mut self) {
        self.func().index_add();
    }

    /// Generates address mul opearation
    pub fn addr_mul(&mut self) {
        self.func().index_mul();
    }


    ////////////////////////////////////////////////////////////
    // Branch operations

    /// Starts generating if-else block using current stack value as condition
    pub fn gen_if(&mut self) {
        panic!("Not implemented for fp256");
        // self.func().gen_call(&self.module_ref().fr().is_true);
        // self.func().gen_wasm_if();
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
        panic!("Not implemented for fp256");
        // self.func().gen_call(&self.module_ref().fr().is_true);
        // self.func().gen_wasm_eqz(&WASMType::I32);
        // self.func().gen_wasm_loop_exit();
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
        self.func().gen_call(&fp256_print);
    }


    ////////////////////////////////////////////////////////////
    // Debugging

    /// Dumps current generator state for debugging
    pub fn debug_dump_state(&self, stage: &str) {
        self.func().debug_dump_state(stage);
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
            debug_output: false
        };
    }
}
