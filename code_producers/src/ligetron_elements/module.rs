
use super::fr::*;
use super::ligetron::*;
use super::template::*;
use super::types::*;
use super::wasm::*;

use num_bigint_dig::BigInt;
use std::rc::Rc;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;

use super::super::wasm_elements::wasm_code_generator::fr_types;
use super::super::wasm_elements::wasm_code_generator::fr_data;
use super::super::wasm_elements::wasm_code_generator::fr_code;


#[derive(Clone)]
pub struct LigetronProducerInfo {
    pub prime: BigInt,
    pub prime_str: String,
    pub fr_memory_size: usize,
    pub size_32_bit: usize,
    pub main_comp_id: usize,
    pub templates: HashMap<usize, TemplateInfo>,
    pub string_table: Vec<String>,
    pub field_tracking: Vec<String>,
    pub debug_output: bool
}


/// Circom WASM module beging generated for Ligetron target
pub struct CircomModule {
    /// Code generation parameters
    info: LigetronProducerInfo,

    /// Underlying WASM module
    module: Rc<RefCell<WASMModule>>,

    /// FR context
    fr_ctx: FRContext,

    /// Ligetron context
    ligetron_ctx: LigetronContext,

    /// Referene to global variable for storing current memory stack frame pointer
    stack_ptr_var: WASMGlobalVariableRef,

    /// String table, contains offset of strings in memory
    string_table: HashMap<usize, usize>,

    /// List of generated module instructions
    instructions: Vec<String>,
}

impl CircomModule {
    /// Creates new Circom WASM module for Ligetrong target
    pub fn new(info: LigetronProducerInfo) -> CircomModule {
        // creating underlying WASM module
        let mut module = WASMModule::new();

        // importing Ligetron functions and creating Ligetron context

        let ligetron_print_type = WASMFunctionType::new().with_params(&[WASMType::I64]);
        let ligetron_print = module.import_function("print",
                                                    ligetron_print_type,
                                                    "env",
                                                    "print");

        let ligetron_print_str_type = WASMFunctionType::new().with_params(&[WASMType::PTR, WASMType::I32]);
        let ligetron_print_str = module.import_function("print_str",
                                                        ligetron_print_str_type,
                                                        "env",
                                                        "print_str");

        let ligetron_dump_memory_type = WASMFunctionType::new().with_params(&[WASMType::PTR, WASMType::I32]);
        let ligetron_dump_memory = module.import_function("dump_memory",
                                                          ligetron_dump_memory_type,
                                                          "env",
                                                          "dump_memory");

        let ligetron_assert_one_type = WASMFunctionType::new().with_params(&[WASMType::I32]);
        let ligetron_assert_one = module.import_function("assert_one",
                                                         ligetron_assert_one_type,
                                                         "env",
                                                         "assert_one");

        let ligetron = LigetronContext {
            print: ligetron_print,
            print_str: ligetron_print_str,
            dump_memory: ligetron_dump_memory,
            assert_one: ligetron_assert_one
        };


        // creating global variable for current memory stack pointer
        let stack_ptr = module.new_global(format!("stack_ptr"),
                                          WASMType::PTR,
                                          format!("(i32.const 0)")); 

        // building string table
        let mut string_table = HashMap::<usize, usize>::new();
        let mut string_offset = info.fr_memory_size;
        for (idx, str) in info.string_table.iter().enumerate() {
            string_table.insert(idx, string_offset);
            string_offset += str.len() + 1;
        }

        return CircomModule {
            info: info,
            module: Rc::new(RefCell::new(module)),
            fr_ctx: FRContext::new(),
            ligetron_ctx: ligetron,
            stack_ptr_var: stack_ptr,
            string_table: string_table,
            instructions: vec![]
        };
    }

    /// Returns size of Circom variable in 32-bit values
    pub fn var_size_32_bit(&self) -> usize {
        return self.info.size_32_bit;
    }

    /// Returns reference to underlying WASM module Rc
    pub fn wasm_module_rc(&self) -> &Rc<RefCell<WASMModule>> {
        return &self.module;
    }

    /// Returns reference to underlying WASM module
    pub fn wasm_module(&self) -> RefMut<WASMModule> {
        return self.module.borrow_mut();
    }

    /// Returns reference to Fr context
    pub fn fr(&self) -> &FRContext {
        return &self.fr_ctx;
    }

    /// Returns reference to Ligetron context
    pub fn ligetron(&self) -> &LigetronContext {
        return &self.ligetron_ctx;
    }

    /// Returns reference to global stack pointer
    pub fn stack_ptr(&self) -> &WASMGlobalVariableRef {
        return &self.stack_ptr_var;
    }

    /// Returns address of global string in string table
    pub fn string_address(&self, idx: usize) -> usize {
        return *self.string_table.get(&idx).expect("can't find string in string table");
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

    /// Returns address of constant in global memory
    pub fn constant_address(&self, const_idx: usize) -> usize {
        return self.calc_constants_start() +
               CircomValueType::FR.size(self.info.size_32_bit) * const_idx;
    }

    /// Calculates start offset of memory stack
    pub fn calc_mem_stack_start(&self) -> usize {
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

    /// Appends instructions into list of module instructions
    pub fn append_instruction(&mut self, insts: &mut Vec<String>) {
        self.instructions.append(insts);
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
        instructions.append(&mut self.wasm_module().generate_globals());

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
}
