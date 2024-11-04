
#[derive(Clone)]
pub enum BuiltinType {
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


/// Represents WASM function type
pub struct FunctionType {
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
    pub fn _to_string(&self) -> String {
        let params_str = self.params_to_string();
        let ret_type_str = self.ret_type_to_string();

        return format!("(func{}{})", params_str, ret_type_str);
    }
}


/// Represents function identifier
pub struct FunctionIdentifier {
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
pub struct FunctionImport {
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
pub struct LocalVariable {
    name: Option<String>,
    type_: BuiltinType,
}

impl LocalVariable {
    /// Creates new local variable
    pub fn new(name: Option<String>, type_: BuiltinType) -> LocalVariable {
        return LocalVariable {
            name: name,
            type_: type_
        };
    }

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
#[derive(Clone)]
pub enum LocalVariableRef {
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


pub struct Module {
    imports: Vec<FunctionImport>,
    instructions: Vec<String>,
}

impl Module {
    /// Creates new empty module
    pub fn new() -> Module {
        return Module {
            imports: Vec::<FunctionImport>::new(),
            instructions: Vec::<String>::new()
        };
    }

    /// Adds function import into generated module. Returns identifier of imported function
    pub fn import_function(&mut self,
                       name: &str,
                       type_: FunctionType,
                       module_name: &str,
                       function_name: &str) -> FunctionIdentifier {
        let import = FunctionImport::new(name, type_, module_name, function_name);
        self.imports.push(import);
        return FunctionIdentifier::new(function_name);
    }

    /// Adds instructions to module
    pub fn add_instructions(&mut self, mut instructions: Vec<String>) {
       self.instructions.append(&mut instructions);
    }

    /// Generates code for module
    pub fn generate(&mut self) -> Vec<String> {
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

        // module instructions
        instructions.append(&mut self.instructions);

        // module footer
        instructions.push(format!(""));
        instructions.push(format!(")"));

        return instructions
    }
}
