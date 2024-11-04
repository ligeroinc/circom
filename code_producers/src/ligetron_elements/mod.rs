
mod core;
mod function_generator;
mod template_generator;

use core::*;
use function_generator::*;
use template_generator::*;

use BuiltinType::*;


#[derive(Clone)]
pub struct LigetronProducerInfo {
    pub main_comp_name: String,
    pub number_of_main_inputs: usize,
    pub number_of_main_outputs: usize
}


pub struct LigetronProducer {
    info: LigetronProducerInfo,
    module: Module,

    /// Generator for current function being generated
    function_gen: Option<FunctionGenerator>,

    /// Generator for current template being generated
    template_gen: Option<TemplateGenerator>
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new(info: &LigetronProducerInfo) -> LigetronProducer {
        return LigetronProducer {
            info: info.clone(),
            module: Module::new(),
            function_gen: None,
            template_gen: None
        };
    }

    /// Generates final code for module
    pub fn generate(&mut self) -> Vec<String> {
        return self.module.generate();
    }


    ////////////////////////////////////////////////////////////
    // Functions

    /// Returns reference to current function generator
    pub fn function_gen(&mut self) -> &mut FunctionGenerator {
        return self.function_gen.as_mut().expect("no current template generator");
    }

    /// Starts generation of new function
    pub fn new_function(&mut self, name: &str) {
        assert!(self.function_gen.is_none());
        self.function_gen = Some(FunctionGenerator::new(name));
    }

    /// Finishes function generation
    pub fn end_function(&mut self) {
        assert!(self.function_gen.is_some());
        let insts = self.function_gen().generate();
        self.module.add_instructions(vec!["".to_string()]);
        self.module.add_instructions(insts);
        self.function_gen = None
    }

    /// Adds return type for current function
    pub fn add_ret_type(&mut self, type_: BuiltinType) {
        self.function_gen().add_ret_type(type_);
    }

    /// Adds new named function parameter for current function.
    /// Returns refence to function parameter.
    pub fn new_param(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        return self.function_gen().new_param(name, type_);
    }

    /// Adds new named local variable. Returns reference to variable.
    pub fn new_named_local(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        return self.function_gen().new_named_local(name, type_);
    }

    /// Adds new unnamed local variable. Returns reference to variable.
    pub fn new_local(&mut self, type_: BuiltinType) -> LocalVariableRef {
        return self.function_gen().new_local(type_);
    }


    ////////////////////////////////////////////////////////////
    // Instructions

    /// Generates instruction in current function
    pub fn gen_inst(&mut self, inst: &str) {
        self.function_gen().add(inst);
    }

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.gen_inst("");
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.gen_empty();
        self.gen_inst(&format!(";; {}", str));
    }

    /// Generates call instruction
    fn gen_call(&mut self, func: FunctionIdentifier) {
        self.gen_inst(&format!("call {}", func.generate()));
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, local: &LocalVariableRef) {
        self.gen_inst(&format!("local.get {}", local.generate()));
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &LocalVariableRef, expr: &str) {
        self.gen_inst(&format!("local.set {} {}", local.generate(), expr));
    }

    /// Generates setting valuf of a local to current value on top of stack
    pub fn gen_local_set(&mut self, local: &LocalVariableRef) {
        self.gen_inst(&format!("local.set {}", local.generate()));
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.gen_inst(&format!("drop"));
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: BuiltinType, value: i64) {
        self.gen_inst(&format!("{}.const {}", type_.generate(), value));
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: BuiltinType) {
        self.gen_inst(&format!("{}.mul", type_.generate()))
    }


    ////////////////////////////////////////////////////////////
    // Utility functions

    /// Generates entry point function for program.
    pub fn generate_entry(&mut self, func_name: String) {
        self.new_function(&func_name);
        self.function_gen().set_export_name(&func_name);

        // we use beginning of global memory (at address 0) to temporarly
        // store information about command line arguments

        // getting number of arguments and buffer size for arguments.
        let args_sizes_get = self.module.import_function(
            "args_sizes_get",
            FunctionType::new().ret_type(I32).params(&[I32, I32]),
            "wasi_snapshot_preview1",
            "args_sizes_get");
        self.gen_comment("getting size of program arguments");
        self.gen_const(I32, 0);     // address to store number of args
        self.gen_const(I32, 4);     // address to store required args buffer size
        self.gen_call(args_sizes_get);

        // removing call return value with error code from stack
        // TODO: check error code
        self.gen_drop();

        // saving number of arguments into local
        let argc = self.new_named_local("argc", I32);
        self.gen_const(I32, 0);
        self.gen_inst("i32.load");
        self.gen_local_set(&argc);

        // saving size of buffer for arguments into local
        let argv_size = self.new_named_local("argv_size", I32);
        self.gen_const(I32, 4);
        self.gen_inst("i32.load");
        self.gen_local_set(&argv_size);

        // getting arguments
        let args_get = self.module.import_function(
            "args_get",
            FunctionType::new().ret_type(I32).params(&[I32, I32]),
            "wasi_snapshot_preview1",
            "args_get");
        self.gen_comment("getting program arguments");
        self.gen_const(I32, 0);     // address to store pointers to arguments
        self.gen_local_get(&argc);
        self.gen_const(I32, 4);
        self.gen_mul(I32);          // address to store arguments buffer
        self.gen_call(args_get);

        // removing call result with error code
        // TODO: check error code
        self.gen_drop();

        // loading parameters on stack
        // TODO: check number of parameters
        for i in 0 .. self.info.number_of_main_inputs {
            // loading address of argument
            self.gen_comment(&format!("loading argument {}", i + 1));
            self.gen_const(I32, (4 * (i + 1)) as i64);
            self.gen_inst("i32.load");

            // loading argument
            self.gen_inst("i64.load");
        }

        // executing main component
        self.gen_comment("executing main component");
        self.gen_call(self.comp_run_function(&self.info.main_comp_name));

        // discarding results of main component
        // TODO: should we handle it?
        for _i in 0 .. self.info.number_of_main_outputs {
            self.gen_drop();
        }

        // calling exit function at the end of entry function
        // TODO: pass correct exit code?
        let proc_exit = self.module.import_function(
            "proc_exit",
            FunctionType::new().params(&[I32]),
            "wasi_snapshot_preview1",
            "proc_exit");
        self.gen_comment("calling exit function");
        self.gen_inst("i32.const 0");
        self.gen_call(proc_exit);

        self.end_function();
    }


    ////////////////////////////////////////////////////////////
    // Templates

    /// Returns reference to template generator
    pub fn template_gen(&self) -> &TemplateGenerator {
        return self.template_gen.as_ref().expect("no current template generator");
    }

    /// Returns reference to mutable template generator
    pub fn template_gen_mut(&mut self) -> &mut TemplateGenerator {
        return self.template_gen.as_mut().expect("no current template generator");
    }

    /// Returns name of function for component with specified name
    pub fn comp_run_function_name(&self, name: &str) -> String {
        return format!("{}_template", name);
    }

    /// Returns component run function for template with specified name
    pub fn comp_run_function(&self, name: &str) -> FunctionIdentifier {
        return FunctionIdentifier::new(&self.comp_run_function_name(name));
    }

    /// Starts generating new template
    pub fn new_template(&mut self, name: &str) {
        // starting genereration of new function for template
        self.new_function(&self.comp_run_function_name(name));

        // creating new template generator
        assert!(self.template_gen.is_none());
        self.template_gen = Some(TemplateGenerator::new());
    }

    /// Finishes generating template
    pub fn end_template(&mut self) {
        assert!(self.template_gen.is_some());

        // loading output signals on stack before return
        self.gen_comment("returning output signals");
        for sig in self.template_gen().output_signals() {
            self.gen_local_get(&sig);
        }

        self.end_function();
        self.template_gen = None;
    }

    /// Adds new input signal
    pub fn new_input_signal(&mut self) {
        // adding function parameter for input signal
        let sig_num = self.template_gen().next_signal_number();
        let sig_var = self.function_gen().new_param(&format!("input_signal_{}", sig_num), I64);
        self.template_gen_mut().add_input_signal(sig_var);
    }

    /// Adds new output signal
    pub fn new_output_signal(&mut self) {
        // adding function return type for output signal
        self.add_ret_type(I64);

        // adding local variable for storing value of output signal
        let sig_num = self.template_gen().next_signal_number();
        let sig_var = self.function_gen().new_named_local(&format!("output_signal_{}", sig_num), I64);
        self.template_gen_mut().add_output_signal(sig_var);
    }

    /// Adds new intermediate signal
    pub fn new_intermediate_signal(&mut self) {
        // adding local variable for storing value of intermediate signal
        let sig_num = self.template_gen().next_signal_number();
        let sig_var = self.function_gen().new_named_local(&format!("intermediate_signal_{}", sig_num), I64);
        self.template_gen_mut().add_output_signal(sig_var);
    }

    /// Returns reference to local variable for signal with specified number
    pub fn signal(&self, sig_num: usize) -> LocalVariableRef {
        return self.template_gen().signal(sig_num);
    }
}

impl Default for LigetronProducerInfo {
    fn default() -> Self {
        return LigetronProducerInfo {
            main_comp_name: "MAIN_COMP".to_string(),
            number_of_main_inputs: 0,
            number_of_main_outputs: 0
        };
    }
}
