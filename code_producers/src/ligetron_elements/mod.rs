
mod fr;
mod func;
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
use memory_stack::*;
use stack::*;
use template_generator::*;
use types::*;
use value::*;
use wasm::*;

use super::wasm_elements::wasm_code_generator::fr_types;
use super::wasm_elements::wasm_code_generator::fr_data;
use super::wasm_elements::wasm_code_generator::fr_code;

use WASMType::*;

use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;


#[derive(Clone)]
pub struct LigetronProducerInfo {
    pub prime_str: String,
    pub size_32_bit: usize,
    pub main_comp_name: String,
    pub number_of_main_inputs: usize,
    pub number_of_main_outputs: usize
}


pub struct LigetronProducer {
    info: LigetronProducerInfo,

    /// FR context
    fr: FRContext,

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
        // creating new module for generated code
        let module = Rc::new(RefCell::new(WASMModule::new()));

        // creating global variable for current memory stack pointer
        let stack_ptr = module.borrow_mut().new_global(format!("stack_ptr"),
                                                       WASMType::PTR,
                                                       format!("(i32.const 0)")); 

        let fr_copy_type = CircomFunctionType::new(vec![CircomValueType::FR],
                                                   vec![CircomValueType::FR]);
        let fr_copy = CircomFunctionRef::new("Fr_copy".to_string(), fr_copy_type);

        let fr_raw_copy_type = CircomFunctionType::new(vec![CircomValueType::WASM(I64)],
                                                       vec![CircomValueType::FR]);
        let fr_raw_copy = CircomFunctionRef::new("Fr_rawCopyS2L".to_string(), fr_raw_copy_type);

        let fr = FRContext {
            copy: fr_copy,
            raw_copy: fr_raw_copy
        };

        return LigetronProducer {
            info: info.clone(),
            fr: fr,
            module_: module,
            stack_ptr: stack_ptr,
            templates: HashMap::<String, CircomFunctionType>::new(),
            instructions: Vec::<String>::new(),
            func_: None,
            template_gen_: None,
        };
    }

    /// Returns reference to module being generated
    pub fn module(&self) -> RefMut<WASMModule> {
        return self.module_.as_ref().borrow_mut();
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
    // Functions

    /// Returns reference to current function
    pub fn func(&self) -> RefMut<CircomFunction> {
        return self.func_
            .as_ref()
            .expect("no current function")
            .borrow_mut();
    }

    /// Starts generation of new function
    pub fn new_function(&mut self, name: &str, n_local_vars: usize) {
        assert!(self.func_.is_none());
        let fgen = CircomFunction::new(self.info.size_32_bit,
                                       self.fr.clone(),
                                       self.module_.clone(),
                                       self.stack_ptr,
                                       name.to_string(),
                                       n_local_vars);
        self.func_ = Some(Rc::new(RefCell::new(fgen)));
    }

    /// Finishes function generation
    pub fn end_function(&mut self, gen_entry_exit: bool) {
        assert!(self.func_.is_some());
        self.instructions.push(format!(""));
        let mut insts = self.func().generate(gen_entry_exit);
        self.instructions.append(&mut insts);
        self.func_ = None
    }

    /// Adds return type for current function
    pub fn add_wasm_ret_type(&mut self, type_: WASMType) {
        self.func().add_wasm_ret_type(type_);
    }

    /// Adds new WASM named function parameter for current function.
    /// Returns refence to function parameter.
    pub fn new_wasm_param(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.func().new_wasm_param(name, type_);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.func().new_wasm_named_local(name, type_);
    }

    /// Adds new WASM unnamed local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, type_: WASMType) -> WASMLocalVariableRef {
        return self.func().new_wasm_local(type_);
    }


    ////////////////////////////////////////////////////////////
    // Instructions

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.func().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func().gen_comment(str);
    }

    /// Generates call instruction
    fn gen_wasm_call(&mut self, func: &WASMFunctionRef) {
        self.func().gen_wasm_call(func);
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, local: &WASMLocalVariableRef) {
        self.func().gen_local_get(local);
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.func().gen_local_set_expr(local, expr);
    }

    /// Generates setting valuf of a local
    pub fn gen_local_set(&mut self, local: &WASMLocalVariableRef) {
        self.func().gen_local_set(local);
    }

    /// Generates getting value of a globa
    pub fn gen_global_get(&mut self, glob: &WASMGlobalVariableRef) {
        self.func().gen_global_get(glob);
    }

    /// Generates setting valuf of a global
    pub fn gen_galobal_set(&mut self, glob: &WASMGlobalVariableRef) {
        self.func().gen_global_set(glob);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.func().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.func().gen_const(type_, value);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        self.func().gen_mul(type_);
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, type_: WASMType) {
        self.func().gen_load(type_);
    }


    ////////////////////////////////////////////////////////////
    // Utility functions

    /// Generates entry point function for program.
    pub fn generate_entry(&mut self, func_name: String) {
        self.new_function(&func_name, 0);
        self.func().set_export_name(&func_name);

        // initializing memory stack pointer
        self.gen_comment("initializing memory stack pointer");
        self.gen_const(PTR, 0);
        self.func().gen_global_set(&self.stack_ptr);

        // we use beginning of memory stack to temporarly
        // store information about command line arguments

        // getting number of arguments and buffer size for arguments.
        let args_sizes_get = self.module().import_function(
            "args_sizes_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_sizes_get");
        self.gen_comment("getting size of program arguments");
        self.func().gen_global_get(&self.stack_ptr);    // address to store number of args
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_const(I32, 4);
        self.func().gen_add(PTR);                       // address to store required args buffer size
        self.func().gen_wasm_call(&args_sizes_get);

        // removing call return value with error code from stack
        // TODO: check error code
        self.gen_drop();

        // saving number of arguments into local
        self.gen_comment("saving number of arguments into argc local");
        let argc = self.new_wasm_named_local("argc", I32);
        self.func().gen_global_get(&self.stack_ptr);
        self.gen_load(I32);
        self.gen_local_set(&argc);

        // saving size of buffer for arguments into local
        self.gen_comment("saving size of buffer for arguments into argv_size local");
        let argv_size = self.new_wasm_named_local("argv_size", I32);
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_const(I32, 4);
        self.func().gen_add(PTR);
        self.gen_load(I32);
        self.gen_local_set(&argv_size);

        // getting arguments
        let args_get = self.module().import_function(
            "args_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_get");
        self.gen_comment("getting program arguments");
        self.func().gen_global_get(&self.stack_ptr);    // address to store pointers to arguments
        self.func().gen_global_get(&self.stack_ptr);
        self.func().gen_local_get(&argc);
        self.func().gen_const(I32, 4);
        self.func().gen_mul(I32);
        self.func().gen_add(PTR);
        self.func().gen_wasm_call(&args_get);

        // removing call result with error code
        // TODO: check error code
        self.gen_drop();

        // saving arguments to local variables
        let mut args = Vec::<WASMLocalVariableRef>::new();
        self.gen_comment("saving arguments to local variables");
        for i in 1 .. self.info.number_of_main_inputs + 1 {
            let loc = self.new_wasm_named_local(&format!("arg_{}", i), I64);
            self.func().gen_global_get(&self.stack_ptr);
            self.func().gen_const(I32, (i * 4) as i64);
            self.func().gen_add(PTR);
            self.gen_load(I64);
            self.gen_local_set(&loc);
            args.push(loc);
        }

        // allocating FR values for results
        self.gen_comment("allocating Fr values for main component results");
        let res_types: Vec<_> = std::iter::repeat([CircomValueType::FR])
            .flatten()
            .take(self.info.number_of_main_inputs)
            .collect();
        let _ = self.func().alloc_mem_stack(res_types);

        // creating FR values from program arguments with Fr_rawCopyS2L function
        self.gen_comment("creating Fr values with porgram arguments");
        let fr_args_types: Vec<_> = std::iter::repeat([CircomValueType::FR])
            .flatten()
            .take(self.info.number_of_main_inputs)
            .collect();
        let fr_args = self.func().alloc_mem_stack(fr_args_types);

        for i in 0 .. self.info.number_of_main_inputs {
            self.func().load_mem_stack_ref(CircomValueType::FR, &fr_args[i]);
            self.func().load_wasm_local(&args[i]);
            self.func().gen_call(&self.fr.raw_copy);
            self.func().drop(1);
        }

        // executing main component
        self.gen_comment("executing main component");
        self.func().gen_call(&self.main_comp_run_function());

        // discarding results of main component
        // TODO: should we handle it?
        self.gen_comment(&format!("discarding main component results: {}", self.info.number_of_main_outputs));
        self.func().drop(self.info.number_of_main_outputs);

        // calling exit function at the end of entry function
        // TODO: pass correct exit code?
        let proc_exit = self.module().import_function(
            "proc_exit",
            WASMFunctionType::new().with_params(&[I32]),
            "wasi_snapshot_preview1",
            "proc_exit");
        self.gen_comment("calling exit function");
        self.gen_const(I32, 0);
        self.gen_wasm_call(&proc_exit);

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

    /// Returns reference to Circom value for signal with specified number
    pub fn signal(&self, sig_num: usize) -> CircomValueRef {
        return self.template_gen().signal(sig_num);
    }

    /// Returns reference to Circom value for variable with specified number
    pub fn circom_var(&self, var_num: usize) -> CircomValueRef {
        return self.func().circom_var(var_num);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Generates loading Circom value to stack from another location
    pub fn gen_circom_load(&mut self, val_ref: &CircomValueRef) {
        self.func().gen_circom_load(val_ref);
    }

    /// Generates saving Circom value located on top of stack to location specified
    /// in the second stack value
    pub fn gen_circom_store(&mut self) {
        self.func().gen_circom_store();
    }

    /// Loads reference to value on top of stack
    pub fn load_ref(&mut self, val: &CircomValueRef) {
        return self.func().load_ref(val);
    }

    /// Drops value from stack
    pub fn drop(&mut self) {
        self.func().drop(1);
    }
}

impl Default for LigetronProducerInfo {
    fn default() -> Self {
        return LigetronProducerInfo {
            prime_str: "bn128".to_string(),
            size_32_bit: 8,
            main_comp_name: "MAIN_COMP".to_string(),
            number_of_main_inputs: 0,
            number_of_main_outputs: 0
        };
    }
}
