
mod fr;
mod func;
mod ligetron;
mod log;
mod memory_stack;
mod stack;
mod template_generator;
mod types;
mod value;
mod wasm;

use serde_json::Value;
pub use template_generator::SignalKind;
pub use template_generator::SignalInfo;

use fr::*;
use func::*;
use ligetron::*;
use log::*;
use template_generator::*;
use types::*;
use value::*;
use wasm::*;

use super::wasm_elements::wasm_code_generator::fr_types;
use super::wasm_elements::wasm_code_generator::fr_data;
use super::wasm_elements::wasm_code_generator::fr_code;

use WASMType::*;

use num_bigint_dig::BigInt;
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;


#[derive(Clone)]
pub struct LigetronProducerInfo {
    pub prime: BigInt,
    pub prime_str: String,
    pub fr_memory_size: usize,
    pub size_32_bit: usize,
    pub main_comp_name: String,
    pub number_of_main_inputs: usize,
    pub number_of_main_outputs: usize,
    pub string_table: Vec<String>,
    pub field_tracking: Vec<String>,
    pub debug_output: bool
}


pub struct LigetronProducer {
    /// Code generation parameters
    info: LigetronProducerInfo,

    /// String table, contains offset of strings in memory
    string_table: HashMap<usize, usize>,

    /// FR context
    fr: FRContext,

    /// Ligetron context
    ligetron: LigetronContext,

    /// WASM module being generated
    module_: Rc<RefCell<WASMModule>>,

    /// Referene to global variable for storing current memory stack frame pointer
    stack_ptr: WASMGlobalVariableRef,

    /// Vector of generated module instructions
    instructions: Vec<String>,

    /// Vector of function types for generated templates
    templates: HashMap<String, CircomFunctionType>,

    /// Generator for current function being generated
    func_: Option<Rc<RefCell<CircomFunction>>>,

    /// Generator for current template being generated
    template_gen_: Option<RefCell<TemplateGenerator>>
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new(info: &LigetronProducerInfo) -> LigetronProducer {
        if info.debug_output {
            set_log_enabled(true);
        }

        debug_log!("LigetronProducer initialization begin");

        // creating new module for generated code
        let module = Rc::new(RefCell::new(WASMModule::new()));

        // creating global variable for current memory stack pointer
        let stack_ptr = module.borrow_mut().new_global(format!("stack_ptr"),
                                                       WASMType::PTR,
                                                       format!("(i32.const 0)")); 

        let ligetron_print_type = WASMFunctionType::new().with_params(&[I64]);
        let ligetron_print = module.borrow_mut().import_function("print",
                                                                 ligetron_print_type,
                                                                 "env",
                                                                 "print");
        let ligetron_print_str_type = WASMFunctionType::new().with_params(&[PTR, I32]);
        let ligetron_print_str = module.borrow_mut().import_function("print_str",
                                                                     ligetron_print_str_type,
                                                                     "env",
                                                                     "print_str");

        let ligetron_dump_memory_type = WASMFunctionType::new().with_params(&[PTR, I32]);
        let ligetron_dump_memory = module.borrow_mut().import_function("dump_memory",
                                                                       ligetron_dump_memory_type,
                                                                       "env",
                                                                       "dump_memory");

        let ligetron_assert_one_type = WASMFunctionType::new().with_params(&[I32]);
        let ligetron_assert_one = module.borrow_mut().import_function("assert_one",
                                                                      ligetron_assert_one_type,
                                                                      "env",
                                                                      "assert_one");

        let ligetron = LigetronContext {
            print: ligetron_print,
            print_str: ligetron_print_str,
            dump_memory: ligetron_dump_memory,
            assert_one: ligetron_assert_one
        };

        // building string table
        let mut string_table = HashMap::<usize, usize>::new();
        let mut string_offset = info.fr_memory_size;
        for (idx, str) in info.string_table.iter().enumerate() {
            string_table.insert(idx, string_offset);
            string_offset += str.len() + 1;
        }

        return LigetronProducer {
            info: info.clone(),
            string_table: string_table,
            fr: FRContext::new(),
            ligetron: ligetron,
            module_: module,
            stack_ptr: stack_ptr,
            templates: HashMap::<String, CircomFunctionType>::new(),
            instructions: Vec::<String>::new(),
            func_: None,
            template_gen_: None,
        };
    }

    ////////////////////////////////////////////////////////////
    // Constants

    /// Loads constant with specified index to stack
    pub fn load_const(&mut self, const_idx: usize) {
        let addr = self.calc_constants_start() +
                   CircomValueType::FR.size(self.info.size_32_bit) * const_idx;
        self.func().load_mem_const(CircomValueType::FR, addr);
    }

    /// Loads u32 constant with specified value to stack
    pub fn load_const_u32(&mut self, val: usize) {
        self.func().load_const(WASMType::I64, val as i64);
        self.debug_dump_state("AAAAAA");
        self.func().gen_call(&self.fr.raw_copy);
        println!("BBBB");
    }


    ////////////////////////////////////////////////////////////
    // Local variables, parameters and return values

    /// Loads reference to local variable or parameter on stack
    pub fn load_local_var_ref(&mut self, var_idx: usize) {
        if self.template_gen_.is_some() {
            // if we are generating template then variable index is real variable index
            // because Circom compiler does not count input signals as function parameters
            let var = self.func().local_var(var_idx);
            self.func().load_local_var_ref(&var);
        } else {
            if var_idx < self.func().params_count() {
                // loading function parameter
                let par = self.func().param(var_idx);
                self.func().load_param_ref(&par);
            } else {
                // loading local variable
                let real_var_idx = var_idx - self.func().params_count();
                let var = self.func().local_var(real_var_idx);
                self.func().load_local_var_ref(&var);
            }
        }
    }

    /// Loads reference to array of local variables or parameter on stack
    pub fn load_local_var_array_ref(&mut self, start_var_idx: usize, size: usize) {
        if self.template_gen_.is_some() {
            // if we are generating template then variable index is real variable index
            // because Circom compiler does not count input signals as function parameters
            let var = self.func().local_var(start_var_idx);
            self.func().load_local_vars_bucket_ref(&var, size);
        } else {
            if start_var_idx < self.func().params_count() {
                panic!("Parater arrays are not supported");
            } else {
                // loading local variable
                let real_var_idx = start_var_idx - self.func().params_count();
                let var = self.func().local_var(real_var_idx);
                self.func().load_local_vars_bucket_ref(&var, size);
            }
        }
    }

    /// Loads reference to return value on stack
    pub fn load_ret_val_ref(&mut self) {
        let ret_val = self.func().ret_val(0);
        self.func().load_ret_val_ref(&ret_val);
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
        let fgen = CircomFunction::new(self.info.size_32_bit,
                                       self.fr.clone(),
                                       self.module_.clone(),
                                       self.stack_ptr,
                                       name.to_string(),
                                       n_local_vars);
        self.func_ = Some(Rc::new(RefCell::new(fgen)));

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
        self.instructions.push(format!(""));
        let mut insts = self.func().generate(gen_entry_exit);
        self.instructions.append(&mut insts);
        self.func_ = None
    }


    ////////////////////////////////////////////////////////////

    /// Generates assert operation
    pub fn assert(&self) {
        self.func().gen_call(&self.fr.is_true);
        self.func().gen_call(&CircomFunctionRef::from_wasm(&self.ligetron.assert_one));
    }

    /// Returns reference to module being generated
    pub fn module(&self) -> RefMut<WASMModule> {
        return self.module_.as_ref().borrow_mut();
    }

    /// Calculates size of string table
    fn calc_string_table_size(&self) -> usize {
        let mut sz = 0;
        for str in &self.info.string_table {
            sz += str.len() + 1;
        }

        return sz;
    }

    /// Calculates sart offset of constants block
    fn calc_constants_start(&self) -> usize {
        return self.info.fr_memory_size +
               self.calc_string_table_size();
    }

    /// Calculates size of constants table
    fn calc_constants_table_size(&self) -> usize {
        return self.info.field_tracking.len() * (self.info.size_32_bit + 2) * 4;
    }

    /// Calculates start offset of memory stack
    fn calc_mem_stack_start(&self) -> usize {
        return self.info.fr_memory_size +
               self.calc_string_table_size() +
               self.calc_constants_table_size();
    }

    /// Generates string table
    fn generate_strings(&self) -> Vec<String> {
        let mut strings = Vec::<String>::new();
        let mut offset = self.info.fr_memory_size;

        for str in &self.info.string_table {
            strings.push(format!("(data (i32.const {}) \"{}\\00\")", offset, str));
            offset += str.len() + 1;
        }

        return strings;
    }

    /// Generates constants data string (copied from wasm_code_generator.rs)
    fn generate_data_constants(&self, constant_list: &Vec<String>) -> String {
        use crate::wasm_elements::wasm_code_generator::wasm_hexa;

        let mut constant_list_data = "".to_string();
        //    For short/long form
        //    let szero = wasm_hexa(producer.get_size_32_bit()*4,&BigInt::from(0));
        for s in constant_list {
            /*
                    // Only long form
                    let n = s.parse::<BigInt>().unwrap();
                    constant_list_data.push_str("\\00\\00\\00\\00\\00\\00\\00\\80");
                    constant_list_data.push_str(&wasm_hexa(producer.get_size_32_bit()*4,&n));
            */
            //      For sort/long or short/montgomery
            let mut n = s.parse::<BigInt>().unwrap();
            let min_int = BigInt::from(-2147483648);
            let max_int = BigInt::from(2147483647);
            let p = self.info.prime.clone();
            let b = ((p.bits() + 63) / 64) * 64;
            let mut r = BigInt::from(1);
            r = r << b;
            n = n % BigInt::clone(&p);
            n = n + BigInt::clone(&p);
            n = n % BigInt::clone(&p);
            let hp = BigInt::clone(&p) / 2;
            let mut nn;
            if BigInt::clone(&n) > hp {
                nn = BigInt::clone(&n) - BigInt::clone(&p);
            } else {
                nn = BigInt::clone(&n);
            }
            /*
                    // short/long
                    if min_int <= nn && nn <= max_int {
                    // It is short
                        if nn < BigInt::from(0) {
                            nn = BigInt::parse_bytes(b"100000000", 16).unwrap() + nn;
                        }
                        constant_list_data.push_str(&wasm_hexa(4,&nn));
                        constant_list_data.push_str("\\00\\00\\00\\00");  // 0000
                        constant_list_data.push_str(&szero);
                    } else {
                    //It is long
                        constant_list_data.push_str("\\00\\00\\00\\00\\00\\00\\00\\80"); // 1000
                        constant_list_data.push_str(&wasm_hexa(producer.get_size_32_bit()*4,&n));
                    }
            */
            //short/montgomery
            if min_int <= nn && nn <= max_int {
                // It is short. We have it in short & Montgomery
                if nn < BigInt::from(0) {
                    nn = BigInt::parse_bytes(b"100000000", 16).unwrap() + nn;
                }
                constant_list_data.push_str(&wasm_hexa(4, &nn));
                constant_list_data.push_str("\\00\\00\\00\\40"); // 0100
            } else {
                //It is long. Only Montgomery
                constant_list_data.push_str("\\00\\00\\00\\00\\00\\00\\00\\C0"); // 1100
            }
            // Montgomery
            // n*R mod P
            n = (n * BigInt::clone(&r)) % BigInt::clone(&p);
            constant_list_data.push_str(&wasm_hexa(self.info.size_32_bit * 4, &n));
        }
        constant_list_data
    }

    /// Generates constatnts data
    fn generate_constants(&self) -> String {
        let start = self.info.fr_memory_size + self.calc_string_table_size();
        let data = self.generate_data_constants(&self.info.field_tracking);
        return format!("(data (i32.const {}) \"{}\")", start, data);
    }

    /// Generates final code for module. Makes this instance invalid
    pub fn generate(&mut self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        // module header 
        instructions.push(format!("(module"));

        // imports
        instructions.push(format!(""));
        instructions.append(&mut self.module().generate_imports());

        // global memory definition
        instructions.push(format!(""));
        instructions.push(format!("(memory 1 1)"));
        instructions.push(format!("(export \"memory\" (memory 0))"));

        // FR types
        instructions.push(format!(""));
        instructions.push(format!(";; Begin of FR types"));
        instructions.append(&mut fr_types(&self.info.prime_str));
        instructions.push(format!(";; End of FR types"));

        // FR data
        instructions.push(format!(""));
        instructions.push(format!(";; Begin of FR data"));
        instructions.append(&mut fr_data(&self.info.prime_str));
        instructions.push(format!(";; End of FR data"));

        // String table
        instructions.append(&mut self.generate_strings());

        // constants table
        instructions.push(self.generate_constants());

        // module globals
        instructions.push(format!(""));
        instructions.append(&mut self.module().generate_globals());

        // FR code
        instructions.push(format!(""));
        instructions.push(format!(";; Begin of FR code"));
        instructions.append(&mut fr_code(&self.info.prime_str));
        instructions.push(format!(";; End of FR code"));

        // module instructions
        instructions.append(&mut self.instructions);

        // module footer
        instructions.push(format!(""));
        instructions.push(format!(")"));

        return instructions;
    }


    ////////////////////////////////////////////////////////////
    // Utility instructions

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func().gen_comment(str);
    }


    ////////////////////////////////////////////////////////////
    // Utility functions

    /// Generates entry point function for program.
    pub fn generate_entry(&mut self, func_name: String) {
        self.new_function(&func_name, 0, 0);
        self.func().set_export_name(&func_name);

        // initializing memory stack pointer
        self.func().gen_comment("initializing memory stack pointer");
        self.func().gen_const(PTR, self.calc_mem_stack_start() as i64);
        self.func().gen_global_set(&self.stack_ptr);

        // we use beginning of memory stack to temporarly
        // store information about command line arguments

        // getting number of arguments and buffer size for arguments.
        let args_sizes_get = self.module().import_function(
            "args_sizes_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_sizes_get");
        self.func().gen_comment("getting size of program arguments");
        self.func().gen_global_get(&self.stack_ptr);    // address to store number of args
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_const(I32, 4);
        self.func().gen_add(PTR);                       // address to store required args buffer size
        self.func().gen_wasm_call(&args_sizes_get);

        // removing call return value with error code from stack
        // TODO: check error code
        self.func().gen_drop();

        // saving number of arguments into local
        self.func().gen_comment("saving number of arguments into argc local");
        let argc = self.func().new_wasm_named_local("argc", I32);
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_load(I32);
        self.func().gen_local_set(&argc);

        // saving size of buffer for arguments into local
        self.func().gen_comment("saving size of buffer for arguments into argv_size local");
        let argv_size = self.func().new_wasm_named_local("argv_size", I32);
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_const(I32, 4);
        self.func().gen_add(PTR);
        self.func().gen_load(I32);
        self.func().gen_local_set(&argv_size);

        // getting arguments
        let args_get = self.module().import_function(
            "args_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_get");
        self.func().gen_comment("getting program arguments");
        self.func().gen_global_get(&self.stack_ptr);    // address to store pointers to arguments
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_local_get(&argc);
        self.func().gen_const(I32, 4);
        self.func().gen_mul(I32);
        self.func().gen_add(PTR);
        self.func().gen_wasm_call(&args_get);

        // removing call result with error code
        // TODO: check error code
        self.func().gen_drop();

        // saving arguments to local variables
        let mut args = Vec::<WASMLocalVariableRef>::new();
        self.func().gen_comment("saving arguments to local variables");
        for i in 1 .. self.info.number_of_main_inputs + 1 {
            let loc = self.func().new_wasm_named_local(&format!("arg_{}", i), I64);
            self.func().gen_global_get(&self.stack_ptr);
            self.func().gen_const(I32, (i * 4) as i64);
            self.func().gen_add(PTR);
            self.func().gen_load(PTR);
            self.func().gen_load(I64);
            self.func().gen_local_set(&loc);
            args.push(loc);
        }

        self.debug_dump_state("BEFORE ENTRY RESULTS");

        let main_comp_func = self.main_comp_run_function();

        // allocating FR array for results
        self.func().gen_comment("allocating Fr array for main component results");
        let ret_vals = self.func().alloc_stack_n(main_comp_func.tp().ret_types());

        self.debug_dump_state("AFTER ENTRY RESULTS");

        // creating FR values from program arguments with Fr_rawCopyS2L function

        self.func().gen_comment("creating Fr array for porgram arguments");
        let fr_args = self.func().alloc_stack_n(main_comp_func.tp().params());

        let mut arg_idx = 0;
        for (fr_arg_idx, par_type) in main_comp_func.tp().params().iter().enumerate() {
            match par_type {
                CircomValueType::FRArray(size) => {
                    for i in 0 .. *size {
                        self.func().load_stack_array_element_ref(&fr_args[fr_arg_idx], i);
                        self.func().load_wasm_local(&args[arg_idx]);
                        self.func().gen_call(&self.fr.raw_copy);
                        self.func().drop(1);
                        arg_idx += 1;
                    }
                }
                CircomValueType::FR => {
                    self.func().load_stack_ref(&fr_args[fr_arg_idx]);
                    self.func().load_wasm_local(&args[arg_idx]);
                    self.func().gen_call(&self.fr.raw_copy);
                    self.func().drop(1);
                    arg_idx += 1;
                }
                CircomValueType::WASM(..) => {
                    panic!("Component function can't have wasm parameters");
                }
            }
        }

        self.debug_dump_state("AFTER ENTRY ARGUMENTS");

        // executing main component
        self.func().gen_comment("executing main component");
        self.func().gen_call(&self.main_comp_run_function());
        self.func().drop(ret_vals.len());

        // calling exit function at the end of entry function
        // TODO: pass correct exit code?
        let proc_exit = self.module().import_function(
            "proc_exit",
            WASMFunctionType::new().with_params(&[I32]),
            "wasi_snapshot_preview1",
            "proc_exit");
        self.func().gen_comment("calling exit function");
        self.func().gen_const(I32, 0);
        self.func().gen_wasm_call(&proc_exit);

        self.end_function(false);
    }


    ////////////////////////////////////////////////////////////
    // Templates

    /// Returns reference to template generator
    pub fn template_gen(&self) -> Ref<TemplateGenerator> {
        return self.template_gen_.as_ref().expect("no current template generator").borrow();
    }

    /// Returns reference to mutable template generator
    pub fn template_gen_mut(&self) -> RefMut<TemplateGenerator> {
        return self.template_gen_.as_ref().expect("no current template generator").borrow_mut();
    }

    /// Returns name of run function for template with specified name
    pub fn templ_run_function_name(&self, name: &str) -> String {
        return format!("{}_template", name);
    }

    /// Returns template run function for template with specified name
    pub fn templ_run_function(&self, name: &str) -> CircomFunctionRef {
        let func_name = self.templ_run_function_name(name);
        let func_type = self.templates.get(name).unwrap();
        return CircomFunctionRef::new(func_name, func_type.clone());
    }

    /// Returns run function for main component
    pub fn main_comp_run_function(&self) -> CircomFunctionRef {
        return self.templ_run_function(&self.info.main_comp_name);
    }

    /// Starts generating new template
    pub fn new_template(&mut self, name: &str,
                        signals: &Vec<SignalInfo>,
                        n_local_vars: usize) {
        // creating new template generator
        assert!(self.template_gen_.is_none());
        let tgen = TemplateGenerator::new(self.info.size_32_bit,
                                          self.fr.clone(),
                                          self.module_.clone(),
                                          name.to_string(),
                                          signals,
                                          n_local_vars,
                                          self.stack_ptr);
        self.template_gen_ = Some(RefCell::new(tgen));

        // saving reference to function generator created by template generator
        assert!(self.func_.is_none());
        self.func_ = Some(self.template_gen_.as_ref().unwrap().borrow_mut().func_gen_rc());
    }

    /// Finishes generating template
    pub fn end_template(&mut self) {
        // generating final template code in function
        self.template_gen_mut().end();

        // finishing function generation and adding it to module
        self.end_function(true);

        // adding template into map of templates
        let templ_name = self.template_gen().name().clone();
        let func_type = self.template_gen().function_type();
        self.templates.insert(templ_name, func_type);

        // removing current template generator
        self.template_gen_ = None;
    }

    /// Loads reference to signal with specified index on stack
    pub fn load_signal_ref(&mut self, circom_sig_idx: usize, size: usize) {
        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig = self.template_gen().signal(real_sig_idx);
            let sig_size = self.template_gen().signal_size(&sig);

            if curr_idx + sig_size > circom_sig_idx {
                if curr_idx == circom_sig_idx && size == sig_size {
                    // reference to signal
                    self.template_gen_mut().load_signal_ref(&sig);
                } else {
                    // reference to subarray inside array signal
                    self.template_gen_mut().load_signal_array_ref(&sig,
                                                                  circom_sig_idx - curr_idx,
                                                                  size);
                }

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Returns reference to return value
    pub fn ret_val(&mut self) -> CircomValueRef {
        return self.func().circom_ret_val(0);
    }

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

    /// Generates return operation
    pub fn gen_return(&mut self) {

    }


    ////////////////////////////////////////////////////////////
    // Logging

    /// Generates logging of string message
    pub fn log_str(&mut self, str_idx: usize) {
        // looking of string offset in string table
        let str_offset = self.string_table.get(&str_idx).unwrap();

        // generating call to Ligetron print_str function
        self.func().gen_comment("logging string");
        self.func().gen_const(PTR, *str_offset as i64);
        self.func().gen_const(I32, self.info.string_table[str_idx].len() as i64);
        self.func().gen_wasm_call(&self.ligetron.print_str);
    }

    /// Generates logging of value located on stack
    pub fn log_val(&mut self) {
        self.func().gen_comment("logging value");

        // loading value located on top of Circom logical stack on WASM stack
        let val = self.func().get_frame().top(0);
        self.func().get_frame().gen_wasm_stack_load(val);

        // loading size of FR value on WASM stack
        self.func().gen_const(I32, (self.info.size_32_bit * 4) as i64);

        // generating call to Ligetron dump_memory function
        self.func().gen_wasm_call(&self.ligetron.dump_memory);

        // removing value located on top of stack
        self.func().drop(1);
    }


    ////////////////////////////////////////////////////////////
    // Debugging

    /// Dumps current generator state for debugging
    pub fn debug_dump_state(&self, stage: &str) {
        if !log::is_log_enabled() {
            return;
        }

        log::debug_log!("");
        log::debug_log!("============================================================");
        log::debug_log!("DUMP STATE BEGIN: {}", stage);
        log::debug_log!("============================================================");

        self.func().debug_dump_state();

        log::debug_log!("DUMP STATE END: {}", stage);
        log::debug_log!("============================================================");
        log::debug_log!("");
    }

    /// Dumps current stack contents to string
    pub fn dump_stack(&self) -> String {
        return self.func().get_frame().dump();
    }
}

impl Default for LigetronProducerInfo {
    fn default() -> Self {
        let prime = BigInt::parse_bytes("21888242871839275222246405745257275088548364400416034343698204186575808495617".as_bytes(), 10)
            .expect("can't parse prime");
        return LigetronProducerInfo {
            prime: prime,
            prime_str: "bn128".to_string(),
            fr_memory_size: 1948,
            size_32_bit: 8,
            main_comp_name: "MAIN_COMP".to_string(),
            number_of_main_inputs: 0,
            number_of_main_outputs: 0,
            string_table: vec![],
            field_tracking: vec![],
            debug_output: false
        };
    }
}
