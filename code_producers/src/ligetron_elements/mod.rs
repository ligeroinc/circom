pub mod ligetron_code_generator;

use std::cell::RefCell;


pub fn merge_code(instructions: Vec<String>) -> String {
    let code = format!("{}\n", instructions.join("\n"));
    code
}

#[derive(Clone)]
enum BuiltinType {
    I32,
    I64
}

impl BuiltinType {
    pub fn generate(&self) -> &'static str {
        match self {
            BuiltinType::I32 => "i32",
            BuiltinType::I64 => "i64"
        }
    }
}

use BuiltinType::*;

/// Represents WASM function type
struct FunctionType {
    ret_type: Option<BuiltinType>,
    params: Vec<BuiltinType>,
}

impl FunctionType {
    /// Constructs new function type without return value and parameters
    pub fn new() -> FunctionType {
        return FunctionType {
            ret_type: None,
            params: vec![]
        };
    }

    /// Creates new function type from this type replacing return value type
    pub fn ret_type(self, ret_type: BuiltinType) -> FunctionType {
        return FunctionType {
            ret_type: Some(ret_type),
            params: self.params
        };
    }

    /// Creates new function type from this type replacing parameter types
    pub fn params(self, params: &[BuiltinType]) -> FunctionType {
        return FunctionType {
            ret_type: self.ret_type,
            params: Vec::from(params)
        };
    }

    /// Converts function return type to WASM string
    pub fn ret_type_to_string(&self) -> String {
        match &self.ret_type {
            Some(ret_type) => format!(" (result {})", ret_type.generate()),
            None => "".to_string()
        }
    }

    /// Converts function parameter types to WASM string
    pub fn params_to_string(&self) -> String {
        if self.params.is_empty() {
            "".to_string()
        } else {
            format!(" (param {})", self.params.iter().map(|t| t.generate()).collect::<Vec<_>>().join(" "))
        }
    }

    /// Converts function type to WASM string
    pub fn to_string(&self) -> String {
        let params_str = self.params_to_string();
        let ret_type_str = self.ret_type_to_string();

        return format!("(func{}{})", params_str, ret_type_str);
    }
}


/// Represents function identifier
struct FunctionIdentifier {
    str: String,
}

impl FunctionIdentifier {
    pub fn new(s: &str) -> FunctionIdentifier {
        return FunctionIdentifier {
            str: s.to_string()
        };
    }

    /// Generates WASM code for function identifier
    pub fn generate(&self) -> String {
        return format!("${}", &self.str)
    }
}


/// Represents function import in WASM model
struct FunctionImport {
    name: String,
    type_: FunctionType,
    module_name: String,
    function_name: String,
}

impl FunctionImport {
    pub fn new(name: &str,
               type_: FunctionType,
               module_name: &str,
               function_name: &str) -> FunctionImport {
        return FunctionImport {
            name: name.to_string(),
            type_: type_,
            module_name: module_name.to_string(),
            function_name: function_name.to_string()
        };
    }

    /// Generates WASM code for importing function
    pub fn generate(&self) -> String {
        return format!("(import \"{}\" \"{}\" (func ${}{}{}))",
                       &self.module_name,
                       &self.function_name,
                       &self.name,
                       &self.type_.params_to_string(),
                       &self.type_.ret_type_to_string());
    }
}


/// Local variable in generated WASM code
struct LocalVariable {
    name: Option<String>,
    type_: BuiltinType,
}

impl LocalVariable {
    /// Generates local variable into WASM code
    pub fn generate(&self) -> String {
        return format!("(local {})", self.generate_name_and_type());
    }

    /// Generates local variable into WASM code as function parameter
    pub fn generate_param(&self) -> String {
        return format!("(param {})", self.generate_name_and_type());
    }

    /// Generates local variable name and type
    fn generate_name_and_type(&self) -> String {
        let name_str = match &self.name {
            Some(s) => format!("${} ", s),
            None => "".to_string()
        };

        return format!("{}{}", name_str, self.type_.generate());
    }
}


/// Reference to local variable
enum LocalVariableRef {
    Name(String),
    Index(usize)
}

impl LocalVariableRef {
    /// Generates local variable reference into WASM
    pub fn generate(&self) -> String {
        match &self {
            LocalVariableRef::Name(name) => format!("${}", name),
            LocalVariableRef::Index(idx) => format!("{}", idx)
        }
    }
}


/// Represents a single function in generated code
struct Function {
    name: String,
    params: Vec<LocalVariable>,
    locals: Vec<LocalVariable>,

    instructions: Vec<String>,
    export_name: Option<String>,
}

impl Function {
    /// Constructs new function
    pub fn new(name: &str) -> Function {
        return Function {
            name: name.to_string(),
            params: Vec::<LocalVariable>::new(),
            locals: Vec::<LocalVariable>::new(),
            instructions: Vec::<String>::new(),
            export_name: None
        }
    }

    /// Adds new named function parameter. Returns refence to function parameter.
    pub fn new_param(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        if !&self.locals.is_empty() {
            panic!("can't add function parameters after local variables");
        }

        let param = LocalVariable {
            name: Some(name.to_string()),
            type_: type_
        };

        self.params.push(param);
        return LocalVariableRef::Name(name.to_string());
    }

    /// Adds new named local variable. Returns reference to variable.
    pub fn new_named_local(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        let local = LocalVariable {
            name: Some(name.to_string()),
            type_: type_
        };

        self.locals.push(local);
        return LocalVariableRef::Name(name.to_string());
    }

    /// Adds new unnamed local variable. Returns reference to variable.
    pub fn new_local(&mut self, type_: BuiltinType) -> LocalVariableRef {
        let local = LocalVariable {
            name: None,
            type_: type_
        };

        let local_idx = self.locals.len() + self.params.len();

        self.locals.push(local);
        return LocalVariableRef::Index(local_idx);
    }

    /// Appends instruction to function body
    pub fn add(&mut self, instruction: &str) {
        self.instructions.push(instruction.to_string());
    }

    /// Generates function code as list of instructions
    pub fn generate(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        // generating function header
        instructions.push(format!("(func ${}", &self.name));

        // generating export name
        match &self.export_name {
            Some(name) => {
                instructions.push(format!("    (export \"{}\")", name))
            }
            None => {}
        }

        // generating parameters
        for param in &self.params {
            instructions.push(format!("    {}", param.generate_param()));
        }

        // generating locals
        instructions.push(format!(""));
        for local in &self.locals {
            instructions.push(format!("    {}", local.generate()));
        }

        // adding function body
        instructions.push(format!(""));
        for inst in &self.instructions {
            instructions.push(format!("    {}", inst))
        }

        // adding terminating ) for function body
        instructions.push(format!(")"));

        return instructions;
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.export_name = Some(name.to_string());
    }
}


pub struct Module {
    imports: Vec<FunctionImport>,
    functions: Vec<Function>,
}

impl Module {
    /// Creates new empty module
    pub fn new() -> Module {
        return Module {
            imports: Vec::<FunctionImport>::new(),
            functions: Vec::<Function>::new()
        };
    }

    /// Adds function import into generated module. Returns identifier of imported function
    fn import_function(&mut self,
                       name: &str,
                       type_: FunctionType,
                       module_name: &str,
                       function_name: &str) -> FunctionIdentifier {
        let import = FunctionImport::new(name, type_, module_name, function_name);
        self.imports.push(import);
        return FunctionIdentifier::new(function_name);
    }

    /// Adds new function into generated module.
    fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }

    /// Generates code for module
    pub fn generate(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        // module header
        instructions.push(format!("(module"));

        // imports
        instructions.push(format!(""));
        for imp in &self.imports {
            instructions.push(imp.generate());
        }

        // global memory definition
        instructions.push(format!(""));
        instructions.push(format!("(memory 1 1)"));

        // functions
        for func in &self.functions {
            instructions.push(format!(""));
            instructions.append(&mut func.generate());
        }

        // module footer
        instructions.push(format!(""));
        instructions.push(format!(")"));

        return instructions
    }
}


pub struct LigetronProducer {
    pub module: RefCell<Module>
}

impl LigetronProducer {
    /// Creates new producer
    pub fn new() -> LigetronProducer {
        return LigetronProducer {
            module: RefCell::new(Module::new())
        };
    }

    /// Generates call instruction
    fn gen_call(&self, func: FunctionIdentifier) -> String {
        return format!("call {}", func.generate());
    }

    /// Generates setting value of a local
    fn gen_local_set(&self, local: LocalVariableRef, expr: &str) -> String {
        return format!("local.set {} {}", local.generate(), expr);
    }

    /// Generates entry point function for program.
    pub fn generate_entry(&self, func_name: String) {
        let mut module = self.module.borrow_mut();

        let mut func = Function::new(&func_name);
        func.set_export_name(&func_name);

        // we use beginning of global memory (at address 0) to temporarly
        // store information about command line arguments

        // getting number of arguments and buffer size of argumens.
        let args_sizes_get = module.import_function(
            "args_sizes_get",
            FunctionType::new().ret_type(I32).params(&[I32, I32]),
            "wasi_snapshot_preview1",
            "args_sizes_get");
        func.add("i32.const 0");      // address to store number of args
        func.add("i32.const 4");      // address to store required args buffer size
        func.add(&self.gen_call(args_sizes_get));

        // saving number of arguments and size of arguments buffer into locals
        let argc = func.new_named_local("argc", I32);
        func.add(&self.gen_local_set(argc, "(i32.load (i32.const 0))"));
        let argv_size = func.new_named_local("argv_size", I32);
        func.add(&self.gen_local_set(argv_size, "(i32.load (i32.const 4))"));

        // calling exit function at the end of entry function
        // TODO: pass correct exit code?
        let proc_exit = module.import_function(
            "proc_exit",
            FunctionType::new().params(&[I32]),
            "wasi_snapshot_preview1",
            "proc_exit");
        func.add("i32.const 0");
        //func.add("local.get $argv_size");
        func.add(&self.gen_call(proc_exit));

        module.add_function(func);
    }
}

impl Default for LigetronProducer {
    fn default() -> Self {
        return LigetronProducer {
            module: RefCell::new(Module::new())
        };
    }
}


/// Helper struct for generating instructions for Ligetron target
pub struct InstructionGenerator {
    module: &Module,
    instructions: Vec<String>
}

impl InstructionGenerator {
    /// Adds instruction to the list of instructions
    pub fn inst(&mut self, inst: &str) {
        self.instructions.push(inst.to_string());
    }

    /// Returns vector of generated instructions
    pub fn instructions(self) -> Vec<String> {
        return self.instructions;
    }
}
