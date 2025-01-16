
use super::ligetron::*;
use super::template::*;
use super::types::*;
use super::wasm::*;
use crate::components::*;

use num_bigint_dig::BigInt;
use std::rc::Rc;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;


#[derive(Clone)]
pub struct LigetronProducerInfo {
    pub const_size: usize,
    pub main_comp_id: usize,
    pub templates: HashMap<usize, TemplateInfo>,
    pub string_table: Vec<String>,
    pub field_tracking: Vec<String>,
    pub debug_output: bool,
    pub io_map: TemplateInstanceIOMap
}


/// Circom WASM module beging generated for Ligetron target
pub struct CircomModule {
    /// Code generation parameters
    info: LigetronProducerInfo,

    /// Underlying WASM module
    module: Rc<RefCell<WASMModule>>,

    /// Ligetron context
    ligetron_ctx: LigetronContext,

    /// Referene to global variable for storing current memory stack frame pointer
    stack_ptr_var: WASMGlobalVariableRef,

    /// String table, contains offset of strings in memory
    string_table: HashMap<usize, usize>,

    /// List of generated module instructions
    instructions: Vec<String>,

    /// Templates runtime information
    templates_info: TemplatesRuntimeInfo
}

impl CircomModule {
    /// Creates new Circom WASM module for Ligetrong target
    pub fn new(info: LigetronProducerInfo) -> CircomModule {
        // creating underlying WASM module
        let mut module = WASMModule::new();

        // creating ligetron context with all required imports
        let ligetron = LigetronContext::new(&mut module);

        // creating global variable for current memory stack pointer
        let stack_ptr = module.new_global(format!("stack_ptr"),
                                          WASMType::PTR,
                                          format!("(i32.const 0)")); 

        // building templates runtime information
        let templates_info = TemplatesRuntimeInfo::build(0, &info);

        // building string table
        let mut string_table = HashMap::<usize, usize>::new();
        let mut string_offset = 0;
        for (idx, str) in info.string_table.iter().enumerate() {
            string_table.insert(idx, string_offset);
            string_offset += str.len() + 1;
        }

        return CircomModule {
            info: info,
            module: Rc::new(RefCell::new(module)),
            ligetron_ctx: ligetron,
            stack_ptr_var: stack_ptr,
            string_table: string_table,
            instructions: vec![],
            templates_info
        };
    }

    /// Returns reference to Circom constants
    pub fn constants(&self) -> &Vec<String> {
        return &self.info.field_tracking;
    }

    /// Returns size of cosntant value stored in global memory
    pub fn const_size(&self) -> usize {
        return self.info.const_size;
    }

    /// Returns reference to underlying WASM module Rc
    pub fn wasm_module_rc(&self) -> &Rc<RefCell<WASMModule>> {
        return &self.module;
    }

    /// Returns reference to underlying WASM module
    pub fn wasm_module(&self) -> RefMut<WASMModule> {
        return self.module.borrow_mut();
    }

    /// Returns reference to Ligetron context
    pub fn ligetron(&self) -> &LigetronContext {
        return &self.ligetron_ctx;
    }

    /// Returns reference to global stack pointer
    pub fn stack_ptr(&self) -> &WASMGlobalVariableRef {
        return &self.stack_ptr_var;
    }

    /// Returns reference to templates runtime info
    pub fn templates_runtime_info(&self) -> &TemplatesRuntimeInfo {
        return &self.templates_info;
    }

    /// Returns size of templates runtime info
    pub fn templates_runtime_info_size(&self) -> usize {
        return self.templates_info.data_size();
    }

    /// Returns offset of templates runtimr info
    pub fn templates_runtime_info_offset(&self) -> usize {
        return 0;
    }

    /// Returns start offset of string table
    pub fn string_table_offset(&self) -> usize {
        return self.templates_runtime_info_size();
    }

    /// Returns address of global string in string table
    pub fn string_address(&self, idx: usize) -> usize {
        return self.string_table_offset() +
               *self.string_table.get(&idx).expect("can't find string in string table");
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
        return self.templates_runtime_info_size() +
               self.calc_string_table_size();
    }

    /// Calculates size of constants table
    fn calc_constants_table_size(&self) -> usize {
        return self.info.field_tracking.len() * self.info.const_size;
    }

    /// Returns address of constant data in global memory
    pub fn constant_address(&self, const_idx: usize) -> usize {
        return self.calc_constants_start() + self.info.const_size * const_idx;
    }

    /// Calculates start offset of memory stack
    pub fn calc_mem_stack_start(&self) -> usize {
        return self.templates_runtime_info_size() +
               self.calc_string_table_size() +
               self.calc_constants_table_size();
    }

    /// Generates string table
    fn generate_strings(&self) -> Vec<String> {
        let mut strings = Vec::<String>::new();
        let mut offset = self.string_table_offset();

        for str in &self.info.string_table {
            strings.push(format!("(data (i32.const {}) \"{}\\00\")", offset, str));
            offset += str.len() + 1;
        }

        return strings;
    }

    fn wasm_hexa(nbytes: usize, num: &BigInt) -> String {
        let inbytes = num.to_str_radix(16).to_string();
        assert!(
            2 * nbytes >= inbytes.len(),
            "the size of memory needs addresses beyond 32 bits long. This circuit cannot be run on WebAssembly\n Try to run circom --c in order to generate c++ code instead"
        );
        let mut temp = "0".repeat(2 * nbytes - inbytes.len());
        temp.push_str(&inbytes);
        let mut res: String = "".to_string();
        for i in 0..nbytes {
            let mut aux = "\\".to_string();
            aux.push_str(&temp[2 * i..2 * i + 2]);
            res.push_str(&aux);
        }
        res
    }

    /// Generates constants data string (copied from wasm_code_generator.rs)
    fn generate_data_constants(&self, constant_list: &Vec<String>) -> String {
        let mut constant_list_data = "".to_string();

        for s in constant_list {
            let n = s.parse::<BigInt>().unwrap();
            constant_list_data.push_str(&Self::wasm_hexa(self.info.const_size, &n));
        }

        constant_list_data
    }

    /// Generates constatnts data
    fn generate_constants(&self) -> String {
        let start = self.calc_constants_start();
        let data = self.generate_data_constants(&self.info.field_tracking);
        return format!("(data (i32.const {}) \"{}\")", start, data);
    }

    /// Appends instructions into list of module instructions
    pub fn append_instruction(&mut self, insts: &mut Vec<String>) {
        self.instructions.append(insts);
    }

    /// Generates table of run functions for templates
    fn generate_run_functions_table(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        instructions.push(format!("(table {} anyfunc)", self.info.templates.len()));

        for templ_idx in 0 .. self.info.templates.len() {
            let templ = self.info.templates.get(&templ_idx)
                .expect("can't find template in templates map");

            let func_name = templ.run_func_name();
            instructions.push(format!("(elem (i32.const {}) ${})", templ_idx, func_name));
        }

        return instructions;
    }

    /// Generates final code for module. Makes this instance invalid
    pub fn generate(&mut self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        // module header 
        instructions.push(format!("(module"));

        // imports
        instructions.push(format!(""));
        instructions.append(&mut self.wasm_module().generate_imports());

        // global memory definition
        instructions.push(format!(""));
        instructions.push(format!("(memory 1 1)"));
        instructions.push(format!("(export \"memory\" (memory 0))"));

        // templates run function table
        instructions.push(format!(""));
        instructions.push(format!(";; templates run functions table"));
        instructions.append(&mut self.generate_run_functions_table());

        // template run function type
        instructions.push(format!(""));
        instructions.push(format!(";; templates run function type"));
        instructions.push(format!("(type $template_run_type (func (param i32)))"));

        // templates runtime info
        instructions.push(format!(""));
        instructions.push(format!(";; templates runtime information"));
        instructions.push(self.templates_info.generate());

        // String table
        instructions.push(format!(""));
        instructions.push(format!(";; string table"));
        instructions.append(&mut self.generate_strings());

        // constants table
        instructions.push(self.generate_constants());

        // module globals
        instructions.push(format!(""));
        instructions.append(&mut self.wasm_module().generate_globals());

        // module instructions
        instructions.append(&mut self.instructions);

        // module footer
        instructions.push(format!(""));
        instructions.push(format!(")"));

        return instructions;
    }

    /// Returns name of run function for template with specified name
    pub fn templ_run_function_name(&self, name: &str) -> String {
        return format!("{}_template", name);
    }

    /// Returns template run function for template with specified ID
    pub fn templ_run_function(&self, id: usize) -> CircomFunctionRef {
        let templ = self.info.templates.get(&id).expect("no template found with specified id");
        let func_name = self.templ_run_function_name(&templ.name());
        return CircomFunctionRef::new(func_name, templ.function_type());
    }

    /// Returns run function for main component
    pub fn main_comp_run_function(&self) -> CircomFunctionRef {
        return self.templ_run_function(self.info.main_comp_id);
    }

    /// Returns template info for main component
    pub fn main_comp_template(&self) -> &TemplateInfo {
        return &self.info.templates[&self.info.main_comp_id];
    }
}
