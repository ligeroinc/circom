
mod func;
mod memory_stack;
mod template_generator;
mod types;
mod wasm;

use serde_json::Value;
pub use template_generator::SignalKind;
pub use template_generator::SignalInfo;

use func::*;
use memory_stack::*;
use template_generator::*;
use types::*;
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

    /// WASM module being generated
    module_: Rc<RefCell<Module>>,

    /// Referene to global variable for storing current memory stack frame pointer
    stack_ptr: GlobalVariableRef,

    /// Vector of generated module instructions
    instructions: Vec<String>,

    /// Vector of function types for generated templates
    templates: HashMap<String, WASMFunctionType>,

    /// Generator for current function being generated
    func_gen_: Option<Rc<RefCell<CircomFunction>>>,

    /// Generator for current template being generated
    template_gen_: Option<RefCell<TemplateGenerator>>
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new(info: &LigetronProducerInfo) -> LigetronProducer {
        // creating new module for generated code
        let module = Rc::new(RefCell::new(Module::new()));

        // creating global variable for current memory stack pointer
        let stack_ptr = module.borrow_mut().new_global(format!("stack_ptr"),
                                                       WASMType::PTR,
                                                       format!("(i32.const 0)")); 

        return LigetronProducer {
            info: info.clone(),
            module_: module,
            stack_ptr: stack_ptr,
            templates: HashMap::<String, WASMFunctionType>::new(),
            instructions: Vec::<String>::new(),
            func_gen_: None,
            template_gen_: None
        };
    }

    /// Returns reference to module being generated
    pub fn module(&self) -> RefMut<Module> {
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

    /// Returns reference to current function generator
    pub fn func_gen(&self) -> RefMut<CircomFunction> {
        return self.func_gen_
            .as_ref()
            .expect("no current function generator")
            .borrow_mut();
    }

    /// Starts generation of new function
    pub fn new_function(&mut self,
                        name: &str,
                        params: &Vec<(String, ValueType)>,
                        ret_types: Vec<ValueType>) {
        assert!(self.func_gen_.is_none());
        let fgen = CircomFunction::new(self.info.size_32_bit,
                                          self.module_.clone(),
                                          self.stack_ptr,
                                          name.to_string(),
                                          params,
                                          ret_types);
        self.func_gen_ = Some(Rc::new(RefCell::new(fgen)));
    }

    /// Finishes function generation
    pub fn end_function(&mut self) {
        assert!(self.func_gen_.is_some());
        self.instructions.push(format!(""));
        let mut insts = self.func_gen().generate();
        self.instructions.append(&mut insts);
        self.func_gen_ = None
    }

    /// Adds return type for current function
    pub fn add_wasm_ret_type(&mut self, type_: WASMType) {
        self.func_gen().add_wasm_ret_type(type_);
    }

    /// Adds new WASM named function parameter for current function.
    /// Returns refence to function parameter.
    pub fn new_wasm_param(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.func_gen().new_wasm_param(name, type_);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.func_gen().new_wasm_named_local(name, type_);
    }

    /// Adds new WASM unnamed local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, type_: WASMType) -> WASMLocalVariableRef {
        return self.func_gen().new_wasm_local(type_);
    }


    ////////////////////////////////////////////////////////////
    // Instructions

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.func_gen().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.func_gen().gen_comment(str);
    }

    /// Generates call instruction
    fn gen_call(&mut self, func: FunctionRef) {
        self.func_gen().gen_call(func);
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, local: &WASMLocalVariableRef) {
        self.func_gen().gen_local_get(local);
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.func_gen().gen_local_set_expr(local, expr);
    }

    /// Generates setting valuf of a local to current value on top of stack
    pub fn gen_local_set(&mut self, local: &WASMLocalVariableRef) {
        self.func_gen().gen_local_set(local);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.func_gen().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.func_gen().gen_const(type_, value);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        self.func_gen().gen_mul(type_);
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, type_: WASMType) {
        self.func_gen().gen_load(type_);
    }


    ////////////////////////////////////////////////////////////
    // Utility functions

    /// Generates entry point function for program.
    pub fn generate_entry(&mut self, func_name: String) {
        self.new_function(&func_name, &vec![], vec![]);
        self.func_gen().set_export_name(&func_name);

        // we use beginning of global memory (at address 0) to temporarly
        // store information about command line arguments

        // getting number of arguments and buffer size for arguments.
        let args_sizes_get = self.module().import_function(
            "args_sizes_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_sizes_get");
        self.gen_comment("getting size of program arguments");
        self.gen_const(PTR, 0);     // address to store number of args
        self.gen_const(PTR, 4);     // address to store required args buffer size
        self.gen_call(args_sizes_get);

        // removing call return value with error code from stack
        // TODO: check error code
        self.gen_drop();

        // saving number of arguments into local
        let argc = self.new_wasm_named_local("argc", I32);
        self.gen_const(PTR, 0);
        self.gen_load(I32);
        self.gen_local_set(&argc);

        // saving size of buffer for arguments into local
        let argv_size = self.new_wasm_named_local("argv_size", I32);
        self.gen_const(PTR, 4);
        self.gen_load(I32);
        self.gen_local_set(&argv_size);

        // getting arguments
        let args_get = self.module().import_function(
            "args_get",
            WASMFunctionType::new().with_ret_type(I32).with_params(&[PTR, PTR]),
            "wasi_snapshot_preview1",
            "args_get");
        self.gen_comment("getting program arguments");
        self.gen_const(PTR, 0);     // address to store pointers to arguments
        self.gen_local_get(&argc);
        self.gen_const(PTR, 4);
        self.gen_mul(PTR);          // address to store arguments buffer
        self.gen_call(args_get);

        // removing call result with error code
        // TODO: check error code
        self.gen_drop();

        // loading parameters on stack
        // TODO: check number of parameters
        for i in 0 .. self.info.number_of_main_inputs {
            // loading address of argument
            self.gen_comment(&format!("loading argument {}", i + 1));
            self.gen_const(PTR, (4 * (i + 1)) as i64);
            self.gen_load(PTR);

            // loading argument
            self.gen_load(I64);
        }

        // executing main component
        self.gen_comment("executing main component");
        self.gen_call(self.main_comp_run_function());

        // discarding results of main component
        // TODO: should we handle it?
        for _i in 0 .. self.info.number_of_main_outputs {
            self.gen_drop();
        }

        // calling exit function at the end of entry function
        // TODO: pass correct exit code?
        let proc_exit = self.module().import_function(
            "proc_exit",
            WASMFunctionType::new().with_params(&[I32]),
            "wasi_snapshot_preview1",
            "proc_exit");
        self.gen_comment("calling exit function");
        self.gen_const(I32, 0);
        self.gen_call(proc_exit);

        self.end_function();
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
    pub fn templ_run_function(&self, name: &str) -> FunctionRef {
        let func_name = self.templ_run_function_name(name);
        let func_type = self.templates.get(name).unwrap();
        return FunctionRef::new(&func_name, func_type.clone());
    }

    /// Returns run function for main component
    pub fn main_comp_run_function(&self) -> FunctionRef {
        return self.templ_run_function(&self.info.main_comp_name);
    }

    /// Starts generating new template
    pub fn new_template(&mut self, name: &str, signals: &Vec<SignalInfo>) {
        // creating new template generator
        assert!(self.template_gen_.is_none());
        let tgen = TemplateGenerator::new(self.info.size_32_bit,
                                          self.module_.clone(),
                                          name.to_string(),
                                          signals,
                                          self.stack_ptr);
        self.template_gen_ = Some(RefCell::new(tgen));

        // saving reference to function generator created by template generator
        assert!(self.func_gen_.is_none());
        self.func_gen_ = Some(self.template_gen_.as_ref().unwrap().borrow_mut().func_gen_rc());
    }

    /// Finishes generating template
    pub fn end_template(&mut self) {
        // generating final template code in function
        self.template_gen_mut().end();

        // finishing function generation and adding it to module
        self.end_function();

        // adding template into map of templates
        let templ_name = self.template_gen().name().clone();
        let func_type = self.template_gen().function_type();
        self.templates.insert(templ_name, func_type);

        // removing current template generator
        self.template_gen_ = None;
    }

    /// Returns reference to local variable for signal with specified number
    pub fn signal(&self, sig_num: usize) -> CircomLocalVariableRef {
        return self.template_gen().signal(sig_num);
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Generates loading of variable value to stack
    pub fn gen_load_var(&mut self, var_ref: &CircomLocalVariableRef) {
        self.func_gen().gen_load_var(var_ref);
    }

    /// Generates saving stack value to variable
    pub fn gen_store_var(&mut self, var_ref: &CircomLocalVariableRef) {
        self.func_gen().gen_store_var(var_ref);
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
