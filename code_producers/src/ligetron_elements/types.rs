
#[derive(Copy, Clone)]
#[derive(PartialEq)]
pub enum WASMType {
    I32,
    I64,
    PTR
}

impl WASMType {
    pub fn generate(&self) -> &'static str {
        match self {
            WASMType::I32 => "i32",
            WASMType::I64 => "i64",
            WASMType::PTR => "i32"
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            WASMType::I32 => "i32",
            WASMType::I64 => "i64",
            WASMType::PTR => "ptr"
        }
    }
}


/// Represents WASM function type
#[derive(Clone)]
pub struct FunctionType {
    params: Vec<WASMType>,
    ret_types: Vec<WASMType>
}

impl FunctionType {
    /// Constructs new function type without return value and parameters
    pub fn new() -> FunctionType {
        return FunctionType {
            params: vec![],
            ret_types: vec![]
        };
    }

    /// Returns reference to vector of parameters
    pub fn params(&self) -> &Vec<WASMType> {
        return &self.params;
    }

    /// Adds parameter to function type
    pub fn add_param(&mut self, type_: WASMType) {
        self.params.push(type_);
    }

    /// Returns reference to vector of return types
    pub fn ret_types(&self) -> &Vec<WASMType> {
        return &self.ret_types;
    }

    /// Adds return type to function type
    pub fn add_ret_type(&mut self, type_: WASMType) {
        self.ret_types.push(type_);
    }

    /// Creates new function type from this type replacing return value type
    pub fn with_ret_type(self, ret_type: WASMType) -> FunctionType {
        return FunctionType {
            params: self.params,
            ret_types: vec![ret_type]
        };
    }

    /// Creates new function type from this type replacing parameter types
    pub fn with_params(self, params: &[WASMType]) -> FunctionType {
        return FunctionType {
            params: Vec::from(params),
            ret_types: self.ret_types
        };
    }

    /// Converts function return types to WASM string
    pub fn ret_types_to_string(&self) -> String {
        if self.ret_types.is_empty() {
            return format!("");
        }

        let str = self.ret_types.iter()
            .map(|t| t.generate().to_string())
            .collect::<Vec<String>>()
            .join(" ");

        return format!(" (result {})", str);
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
        let ret_type_str = self.ret_types_to_string();

        return format!("(func{}{})", params_str, ret_type_str);
    }
}


/// Reference to a function
pub struct FunctionRef {
    /// Function name
    name: String,

    /// Function type
    pub type_: FunctionType
}

impl FunctionRef {
    pub fn new(name: &str, type_: FunctionType) -> FunctionRef {
        return FunctionRef {
            name: name.to_string(),
            type_: type_
        };
    }

    /// Generates WASM code for function identifier
    pub fn generate(&self) -> String {
        return format!("${}", &self.name)
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
                       &self.type_.ret_types_to_string());
    }
}

